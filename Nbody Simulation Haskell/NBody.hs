{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import System.Environment
import Data.List
import Data.Function
import qualified Data.Foldable as F
import Control.Applicative

infixr 8 <+>
infixr 8 <->

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  putStr.unlines.applyBH args $ lines contents

data Vec a = Vec [a] | InvVec deriving (Show,Eq)

class Vector f where
  (<+>) :: Num a => f a -> f a -> f a
  (<->) :: Num a => f a -> f a -> f a
  (<.>) :: Num a => a -> f a -> f a
  dist2 :: (Num a , Floating a) => f a -> f a -> a
  zero :: (Num a, Eq a) => a -> f a
  quad :: (Ord a, Num a, Fractional a) => f a -> a -> f a -> f a

instance Vector Vec where
  _ <+> InvVec = error "Invalid Vec detected"
  InvVec <+> _ = error "Invalid Vec detected"
  (<+>) (Vec v) (Vec v') | length v == length v' = Vec (zipWith (+) v v') | otherwise = InvVec
  _ <-> InvVec = error "Invalid Vec detected"
  InvVec <-> _ = error "Invalid Vec detected"
  (<->) (Vec v) (Vec v') | length v == length v' = Vec (zipWith (-) v v') | otherwise = InvVec
  _ <.> InvVec = error "Invalid Vec detected"
  (<.>) dt (Vec v') = Vec (map (dt*) v')
  dist2 _ InvVec = error "Invalid Vec detected"
  dist2 InvVec _ = error "Invalid Vec detected"
  dist2 (Vec v) (Vec v') | length v == length v' = sum.map (**2) $ zipWith (-) v v' | otherwise = error "Length Mismatch"
  zero a = Vec (q a) where q b | b == 0 = [] | otherwise = 0 : q (b-1)
  quad _ _ InvVec = error "Invalid Vec detected"
  quad InvVec _ _ = error "Invalid Vec detected"
  quad (Vec v) r (Vec v') | length v == length v' = Vec (map (\x->if x then r/2 else -r/2) $ zipWith (<) v v')| otherwise = error "Length Mismatch"

instance Functor Vec where
  fmap _ InvVec = InvVec
  fmap f (Vec v) = Vec (map f v)

instance Applicative Vec where
  pure a = Vec [a]
  InvVec <*> _ = InvVec
  _ <*> InvVec = InvVec
  (Vec f) <*> (Vec k) = Vec (map (\(func,n)->func n) (zip f k))

instance Monad Vec where
  InvVec >>= _ = InvVec
  Vec [] >>= _ = Vec []
  Vec v >>= f = Vec (concatMap ((\(Vec a) -> a).f) v)
  return x = Vec [x]

instance Vector [] where
  (<+>) = zipWith (+)
  (<->) = zipWith (-)
  (<.>) dt = map (dt*)
  dist2 v v' = sum.map (**2) $ zipWith (-) v v'
  zero a | a == 0 = [] | otherwise = 0 : zero (a-1)
  quad v r v' = map (\x->if x then r/2 else -r/2) $ zipWith (<) v v'

instance Show (Body []) where
  show (Body a b c) = show a ++ show b ++ show c
instance Show (Body Vec) where
  show (Body a b c) = show a ++ show b ++ show c

applyBH :: [String] -> [String] -> [String]
applyBH [tm,dt] (cnt:r:bodies) =
  let bodiesIn = map (readBody.words) $ take (read cnt) bodies
      bodiesOut = barnesHut (read tm) (read dt) (read r) bodiesIn in
  cnt:r:map show bodiesOut
applyBH _ _ = error "Invalid input"

data Body v where
  Body :: Vector v => v Double -> v Double -> Double -> Body v

readBody :: [String] -> Body Vec
readBody (x:y:z:vx:vy:vz:m:_) = Body (Vec [read x,read y, read z]) (Vec [read vx,read vy, read vz]) (read m)
readBody _ = error "Invalid input"

barnesHut :: (Vector v, Eq (v Double)) => Double -> Double -> Double -> [Body v] -> [Body v]
barnesHut tm dt r bodies = snd $ until ((<=0).fst) (tick dt r) (tm, bodies)

tick :: (Vector v, Eq (v Double)) => Double -> Double -> (Double, [Body v]) -> (Double, [Body v])
tick dt r (tm, bodies) =
  let tree = makeQT (zero 3) r (\(Body p _ _)->p) summary bodies in
  (tm-dt, map (updateBody tree dt) bodies)

updateBody :: (Vector v, Eq (v Double)) => QT (Body v) -> Double -> Body v -> Body v
updateBody tree dt (Body p v m) =
  let calcplus a b c = c <+> calcAcc a b
      sumOk r (Body c _ _) = r/dist p c < 0.5
      aList = F.foldr (calcplus p) (zero 3) (Foldtree sumOk tree)
      v' = v <+> dt <.> aList
      p' = p <+> dt <.> v' in
  Body p' v' m

summary :: Vector v => [Body v] -> Body v
summary bodies =
  let masses = map (\(Body _ _ m)->m) bodies
      totalM = sum masses
      newpos = (1/totalM) <.> foldr ((<+>).(\(Body p _ m)->m<.>p)) (zero 3) bodies in
  Body newpos (zero 3) totalM

dist :: (Vector v) => v Double -> v Double -> Double
dist v v' = sqrt (dist2 v v')

calcAcc :: (Vector v, Eq (v Double)) => v Double -> Body v ->  v Double
calcAcc p (Body p' _ m') =
  let constG = 0.0000000000667
      accel = constG * m' / (dist p p' ** 3) in
  if p==p' then zero 3 else accel <.> (p' <-> p)

data QT a = QT Double a [QT a] | LeafQT Double a
data Foldtree a = Foldtree (Double->a->Bool) (QT a)

instance F.Foldable Foldtree where
  foldr _ acc (Foldtree _ (QT _ _ [])) = acc
  foldr facc acc (Foldtree _ (LeafQT _ b)) = facc b acc
  foldr facc acc (Foldtree f (QT r b (x:xs)))
      | f r b = facc b acc
      | otherwise = F.foldr facc (F.foldr facc acc (Foldtree f (QT r b xs))) (Foldtree f x)

makeQT :: (Vector v, Eq (v Double)) => v Double-> Double -> (a-> v Double)-> ([a]->a) -> [a] -> QT a
makeQT _ _ _ _ [] = error "No data to make tree"
makeQT _ r _ _ [x] = LeafQT r x
makeQT c r toXY f nodes =
  let qcheck b = quad c r (toXY b)
      quads = groupBy ((==) `Data.Function.on` qcheck) nodes
      newC n = c <+> (qcheck.head) n in
  QT r (f nodes) $ map (\n->makeQT (newC n) (r/2) toXY f n) quads
