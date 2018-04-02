import Data.List
import Control.Applicative

data Dist a = Dist [(a, Double)] deriving (Show, Eq)

-- Part 1 --

certainly :: a -> Dist a
certainly x = Dist [(x,1.0)]-- <<<<<<<<<< START HERE

returnarr :: [(a,Double)] -> Double
returnarr [] = 0
returnarr ((_,b):xs) = b + returnarr xs

dive :: [(a,Double)] -> Double -> [(a,Double)]
dive [] _ = []
dive ((a,b):xs) todiv = [(a,b/todiv)] ++ (dive xs todiv) 

scale :: Dist a -> Dist a
scale (Dist a)= Dist (dive a (returnarr a))

-- Part 2 --
merge :: [Dist a] -> [(a,Double)]
merge [] = []
merge ((Dist a):xs) = a ++ merge xs

uniform :: [a] -> Dist a
uniform [] = Dist []
uniform a = scale (Dist (merge (map certainly a)))

die :: Dist Int
die = uniform [1,2,3,4,5,6]
  --

coin :: Dist Char
coin = uniform ['H','T']

x = [[("abc",2.0),("abc",2.0)],[("def",3.0)]]
-- Part 3 --
dosomeo :: [(a,Double)] -> [(a,Double)]
dosomeo [] = []
dosomeo a@((b,_):xs) = [(b,returnarr a)]

dosome :: [[(a,Double)]] -> [(a,Double)]
dosome [] = []
dosome a = concat (map dosomeo a) 

norm :: Ord a => Dist a -> Dist a
norm (Dist a) = Dist (dosome (groupBy (\(x,_) (y,_)-> x==y)  (sortBy (\(x,_) (y,_)-> compare x y) a)))

-- Part 4 --

(??) :: (a -> Bool) -> Dist a -> Double
(??) f (Dist a) = returnarr ((filter (f.fst) a)) 

-- Part 5 --

instance Functor Dist where
  fmap f (Dist x) = Dist (map (\(a,b)->(f a,b)) x)

-- Part 6 --

pairing a b = [(ax,bx) | ax <- a, bx <- b]

doApp :: [(((a->b),Double),(a,Double))] -> [(b,Double)]
doApp [] = []
doApp (((f,a),(b,c)):xs) = [((f b),(a*c))] ++ doApp xs

instance Applicative Dist where
  pure x = certainly x
  (<*>) (Dist a) (Dist b)= Dist (doApp (pairing a b)) 

-- Part 7 --

helpsel :: [Dist [a]] -> Dist [a]
helpsel [x] = x 
helpsel (x:xs) = (++) <$> x <*> helpsel xs

selectI :: Int -> Dist a -> Dist [a]
selectI 0 _ = Dist []
selectI n (Dist a)= helpsel (replicate n (Dist (map (\(a,b)->([a],b)) a)))

-- Part 8 --

instance (Ord a, Num a) => Num (Dist a) where
  (+) a b = norm $ (+) <$> a <*> b  
  (*) a b = norm $ (*) <$> a <*> b
  abs a = (abs) <$> a
  negate a = (*(-1)) <$> a
  signum a = Dist [(-1,((<0) ?? a)),(0,((==0) ?? a)),(1, ((>0)?? a))]
  fromInteger a = certainly (fromInteger a)

-- Part 9 --

condDist :: Eq a => Dist a -> (Dist (a, Dist a))
condDist k@(Dist m) = Dist (map (fone k) m) 
  where
    fone a@(Dist b) c@(e,d) = ((e, (func b c)),d)
    func arr tup = scale (Dist (delete tup arr)) 

-- Part 10 --

selectD' :: Ord a => Int -> Dist a -> Dist ([a],Dist a)
selectD' 0 c = return ([],c)
selectD' n c = do 
  (x,c1)  <- condDist c
  (xs,c2) <- selectD' (n-1) c1
  return (x:xs, c2)

selectD :: Ord a => Int -> Dist a -> Dist [a]
selectD n c = norm $ fst <$> (selectD' n c)

outofbox :: Dist a -> [(a,Double)]
outofbox (Dist a) = a

applyfunc :: [(a,Double)] -> (a->Dist b) -> [(b,Double)]
applyfunc [] _ = []
applyfunc ((x,y):xs) f = (outofbox (f x)) ++ (applyfunc xs f) 

instance Monad Dist where
  (>>=) (Dist a) f = scale (Dist (applyfunc a f)) 
  return a = certainly a

-- Part 11 --

data DistF a b = DistF (Dist a->Dist (b,Dist a))

selectD'' :: Ord a => Int -> DistF a [a]
selectD'' 0 = return []
selectD'' n = do 
  x <- DistF condDist
  xs <- selectD'' (n-1) 
  return (x:xs)

applyDistF :: DistF a b -> Dist a -> Dist b
applyDistF = undefined

-- Part 12 --

instance Functor (DistF a) where 
  fmap = undefined
instance Applicative (DistF a) where 
  pure = undefined
  (<*>) = undefined
instance Ord a => Monad (DistF a) where
  (>>=) = undefined
  return = undefined

-- Part 13 --

pickNumber :: Int -> Dist Int
pickNumber = undefined 

--TESTS--

tests :: [[(String,String)]]
tests = [
    [(show $ certainly "Abc", show $ Dist [("Abc",1.0)]),
     (show $ scale (Dist [("Abc",1),("Def",1)]), show $ Dist [("Abc",0.5),("Def",0.5)]), 
     (show $ scale (Dist [("Abc",100),("Def",300)]), show $ Dist [("Abc",0.25),("Def",0.75)])]
  , [(show $ uniform ["Abc", "Def"], show $ Dist [("Abc",0.5),("Def",0.5)]),
  (show $ uniform ["Ab", "De", "Abc", "Def"], show $ Dist [("Ab",0.25),("De",0.25),("Abc",0.25),("Def",0.25)]),
  (show $ die, show $ Dist [((1::Integer),0.16666666666666666),(2,0.16666666666666666),(3,0.16666666666666666),(4,0.16666666666666666),(5,0.16666666666666666),(6,0.16666666666666666)]),
  (show $ coin, show $ Dist [('H',0.5),('T',0.5)])]
  , [(show $ norm (Dist [("Abc", 0.25), ("Def", 0.25), ("Abc", 0.5)]), show $ Dist [("Abc", 0.75), ("Def", 0.25)]),
  (show $ norm (Dist [("Abc", 0.25), ("Abc", 0.25), ("Abc", 0.5)]), show $ Dist [("Abc", 1)])]
  , [(show $ (=='H') ?? coin, "0.5"),(show $ (<=3) ?? die, "0.5")]
  , [(show $ (+2) <$> die, show $ Dist [((3::Integer),0.16666666666666666),(4,0.16666666666666666),(5,0.16666666666666666),(6,0.16666666666666666),(7,0.16666666666666666),(8,0.16666666666666666)])]
  , [(show $ norm $ (+) <$> die <*> die, show $ Dist [((2::Integer),2.7777777777777776e-2),(3,5.555555555555555e-2),(4,8.333333333333333e-2),(5,0.1111111111111111),(6,0.1388888888888889),(7,0.16666666666666669),(8,0.1388888888888889),(9,0.1111111111111111),(10,8.333333333333333e-2),(11,5.555555555555555e-2),(12,2.7777777777777776e-2)])]
  , [(show $ norm $ sum <$> selectI 2 die, show $ Dist [((2::Integer),2.7777777777777776e-2),(3,5.555555555555555e-2),(4,8.333333333333333e-2),(5,0.1111111111111111),(6,0.1388888888888889),(7,0.16666666666666669),(8,0.1388888888888889),(9,0.1111111111111111),(10,8.333333333333333e-2),(11,5.555555555555555e-2),(12,2.7777777777777776e-2)])]
  , [(show $ die + die, show $ Dist [((2::Integer),2.7777777777777776e-2),(3,5.555555555555555e-2),(4,8.333333333333333e-2),(5,0.1111111111111111),(6,0.1388888888888889),(7,0.16666666666666669),(8,0.1388888888888889),(9,0.1111111111111111),(10,8.333333333333333e-2),(11,5.555555555555555e-2),(12,2.7777777777777776e-2)]),
  (show $ die * die, show $ Dist [(1,2.7777777777777776e-2),((2::Integer),5.555555555555555e-2),(3,5.555555555555555e-2),(4,8.333333333333333e-2),(5,5.555555555555555e-2),(6,0.1111111111111111),(8,5.555555555555555e-2),(9,2.7777777777777776e-2),(10,5.555555555555555e-2),(12,0.1111111111111111),(15,5.555555555555555e-2),(16,2.7777777777777776e-2),(18,5.555555555555555e-2),(20,5.555555555555555e-2),(24,5.555555555555555e-2),(25,2.7777777777777776e-2),(30,5.555555555555555e-2),(36,2.7777777777777776e-2)]),
  (show $ negate $ uniform [-2..2::Int], show $ Dist [((2::Integer),0.2),(1,0.2),(0,0.2),(-1,0.2),(-2,0.2)]),
  (show $ signum $ uniform [-2..2::Int], show $ Dist [((-1::Integer),0.4),(0,0.2),(1,0.4)]),(show $ ((fromInteger 10)::Dist Int), show $ Dist [((10::Integer),1.0)])]
  , [(show $ condDist $ Dist [('A', 0.3), ('B', 0.3), ('C', 0.4)], show $ Dist [(('A',Dist [('B',0.4285714285714286),('C',0.5714285714285715)]),0.3),(('B',Dist [('A',0.4285714285714286),('C',0.5714285714285715)]),0.3),(('C',Dist [('A',0.5),('B',0.5)]),0.4)]),
  (show $ condDist die, show $ Dist [(((1::Integer),Dist [((2::Integer),0.2),(3,0.2),(4,0.2),(5,0.2),(6,0.2)]),0.16666666666666666),((2,Dist [(1,0.2),(3,0.2),(4,0.2),(5,0.2),(6,0.2)]),0.16666666666666666),((3,Dist [(1,0.2),(2,0.2),(4,0.2),(5,0.2),(6,0.2)]),0.16666666666666666),((4,Dist [(1,0.2),(2,0.2),(3,0.2),(5,0.2),(6,0.2)]),0.16666666666666666),((5,Dist [(1,0.2),(2,0.2),(3,0.2),(4,0.2),(6,0.2)]),0.16666666666666666),((6,Dist [(1,0.2),(2,0.2),(3,0.2),(4,0.2),(5,0.2)]),0.16666666666666666)])]
  , [(show $ norm $ sum <$> selectD 2 die, show $ Dist [((3::Integer),6.666666666666664e-2),(4,6.666666666666664e-2),(5,0.13333333333333328),(6,0.13333333333333328),(7,0.19999999999999993),(8,0.13333333333333328),(9,0.13333333333333328),(10,6.666666666666664e-2),(11,6.666666666666664e-2)]),
  (show $ norm $ sum <$> selectD 3 die,show $ Dist [(6,4.9999999999999996e-2),((7::Integer),4.9999999999999996e-2),(8,9.999999999999999e-2),(9,0.15),(10,0.15),(11,0.15),(12,0.15),(13,9.999999999999999e-2),(14,4.9999999999999996e-2),(15,4.9999999999999996e-2)])]
  , [(show $ applyDistF (DistF condDist) die, show die)]
  , [(show $ norm $ sum <$> applyDistF (selectD'' 3) die, show $ Dist [((6::Integer),4.9999999999999996e-2),(7,4.9999999999999996e-2),(8,9.999999999999999e-2),(9,0.15),(10,0.15),(11,0.15),(12,0.15),(13,9.999999999999999e-2),(14,4.9999999999999996e-2),(15,4.9999999999999996e-2)])]
  , [(show $ pickNumber 15, show $ Dist [((1::Integer),0.1900793650794215),(2,0.533015873015843),(3,0.27690476190486657)]),(show $ pickNumber 10, show $ Dist [((1::Integer),0.6965079365066251),(2,0.24666666666675455),(3,5.682539682538513e-2)]),(show $ pickNumber 100, show $ Dist [(1,5.634920634920748e-3),(2,0.23412698412706504),((3::Integer),0.7602380952362844)])]]

test2str :: (Int,[(String,String)]) -> String 
test2str (x,y) = 
  case dropWhile (\(_,(a,b))->a==b) (zip [1::Integer ..] y) of  
    [] -> "Part "++(show x)++" works on given examples."
    ((n,(act,ex)):_) -> "---------------------\nPart "++(show x)++" FAILED\n  Test case "++(show n)++" failed\n  Expected Output: "++ex++"\n    Actual Output: "++act++"\n---------------------"

main :: IO ()
main = putStr $ unlines $ map test2str $ zip [1..] tests

