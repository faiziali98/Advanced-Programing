import Data.List (find,nub)
import Data.Char (isAlpha)

data Token = Word String | Blank | HypWord String deriving (Eq,Show)

-- Part 1 --
tokenize :: String -> Token
tokenize a = Word a

str2line :: String -> [Token]
str2line a = map (tokenize) (words a)


-- Part 2 --

unword :: Token -> String
unword (Word a) = a ++ " "
unword (HypWord a) = a ++ "- "

line2str :: [Token] -> String
line2str [(Word x)] = x
line2str [(HypWord x)] = x ++ "-"
line2str (x:xs) = (unword x) ++ (line2str xs)


-- Part 3 --
tokLen :: Token -> Int
tokLen (Blank) = 1
tokLen (Word x) = length x
tokLen (HypWord x) = (length x) + 1

-- Part 4 --

lineLen :: [Token] -> Int
lineLen [] = 0
lineLen x = sum (map (tokLen) x) + (length x) - 1

-- Part 5 --

partLine :: Int -> ([Token],[Token]) -> ([Token],[Token])
partLine _ (x,[]) = (x,[])
partLine n q@(a,(x:xs))
          | (tokLen x) == n = ((a++[x]),xs)
          | (tokLen x)<n = partLine (n-((tokLen x)+1)) ((a++[x]),xs)
          | otherwise = q  

breakLine :: Int -> [Token] -> ([Token],[Token])
breakLine _ [] = ([],[])
breakLine n a@(x:xs) 
              | (lineLen a) < n = (a, [])
              | (tokLen x) > n  = ([],a)
              | otherwise = partLine (n-((tokLen x)+1)) ([x],xs)

-- Part 6 --
mergers :: [String] -> [(String,String)]
mergers [] = []
mergers [x] = []
mergers [x,y] = [(x,y)]
mergers (x:y:xs) = [(x,(concat ([y]++xs)))] ++ mergers ([(concat ([x]++[y]))] ++ xs) 

-- Part 7 --

putspar :: [(String,String)] -> String -> [(String,String)]
putspar [] c = []
putspar [(a,b)] c = [(a,(b++c))]
putspar ((a,b):xs) c = [(a,(b++c))] ++ putspar xs c

helphy :: [(String,String)] -> [(Token,Token)]
helphy [] = []
helphy ((x,y):xs) = [(HypWord x,Word y)] ++ helphy xs

retarr :: [(String,[String])] -> String -> [String]
retarr [(x,b)] n | x == n = b | otherwise = []
retarr ((a,b):xs) n | a==n = b | otherwise =  retarr xs n

hyphenate :: [(String, [String])] -> Token -> [(Token,Token)]
hyphenate _ Blank  = []
hyphenate a or@(Word b) = helphy (putspar (mergers (a `retarr` (arr!!0))) (arr!!1))
    where arr =  retst (span (isAlpha) b)
          retst (a,b) = [a,b]

-- Part 8 --

rettup :: ([Token],[Token]) -> [Token]
rettup ([],_) = []
rettup (a,_) = a

retfirststr :: ([Token],[Token]) -> Token
retfirststr (_,[]) = Blank
retfirststr (a,(x:xs)) = x

retrem :: ([Token],[Token]) -> [Token]
retrem (_,[]) = []
retrem (a,(x:xs)) = xs

something :: [Token] -> [(Token,Token)] -> Int -> [Token] -> [([Token],[Token])]
something [] _ _ _ = []
something _ [] _ _ = [] 
something a b@((d,e):xs) n g
                | ((lineLen a) + (tokLen d)) < n = [(a++[d], [e]++g)] ++ something a xs n g
                | otherwise = something a xs n g

lineBreaks :: [(String, [String])] -> Int -> [Token] -> [([Token],[Token])]
lineBreaks a b c = [p] ++ something (rettup p) (hyphenate a (retfirststr p)) b (retrem p)
           where p = breakLine b c


-- Part 9 --
con2arr :: (a,a) -> [a]
con2arr (g,b) = [g] ++ [b]

dosomthing :: Int -> a -> [a] -> [[a]]
dosomthing 0 e b = [[e]++b]
dosomthing e b c = [(kk!!0) ++ [b] ++ (kk!!1)] ++ dosomthing (e-1) b c   
          where kk = con2arr (splitAt e c)

insertions :: a -> [a] -> [[a]]
insertions a [] = [[a]]
insertions a b@(x:xs) = reverse (dosomthing (length b) a b) 

-- Part 10 --
--dosomthingb :: Int -> a -> [a] -> [[a]]
--dosomthingb 0 e b = []
--dosomthingb e b c = [(kk!!0) ++ [b] ++ (kk!!1)] ++ dosomthingb (e-1) b c   
--          where kk = con2arr (splitAt e c)

--insertionsb :: a -> [a] -> [[a]]
--insertionsb a [] = [[a]]
--insertionsb a b = reverse (dosomthingb ((length b)-1) a b) 


--insertanceBlank :: Int -> [[Token]] -> [[Token]]
--insertanceBlank a [([]):xs] = insertanceBlank (a-1) [xs] 
--insertanceBlank 0 _ = []
--insertanceBlank a (x:xs) = insertionsb Blank x 

insertBlanks :: Int -> [Token] -> [[Token]]
--insertBlanks 0 b = [b]
--insertBlanks a b = map  (insertionsb Blank) (insertionsb Blank b)
insertBlanks = undefined

-- Part 11 --

blankDistances :: [Token] -> [Int]
blankDistances = undefined

-- Part 12 --

avg :: [Double] -> Double 
avg = undefined

var :: [Double] -> Double 
var = undefined

-- Part 13 --

data Costs = Costs Double Double Double Double deriving (Eq,Show)

lineBadness :: Costs -> [Token] -> Double
lineBadness = undefined

-- Part 14 --

bestLineBreak :: Costs -> [(String, [String])] -> Int -> [Token] -> Maybe ([Token],[Token]) 
bestLineBreak = undefined

-- Part 15 --

justifyLine :: Costs -> [(String, [String])] -> Int -> [Token] -> [[Token]]
justifyLine = undefined

-- Part 16 --

justifyText :: Costs -> [(String, [String])] -> Int -> String -> [String]
justifyText = undefined

--TESTS--

text :: String
text = "He who controls the past controls the future. He who controls the present controls the past."

defaultCosts :: Costs
defaultCosts = Costs 1 1 0.5 0.5

enHyp :: [(String, [String])]
enHyp = [("controls",["co","nt","ro","ls"]), ("future",["fu","tu","re"]),("present",["pre","se","nt"])]

tests :: [[(String,String)]]
tests = [
    [(show $ str2line text, show [Word "He",Word "who", Word "controls", Word "the", Word "past", Word "controls", Word "the", Word "future.", Word "He", Word "who", Word "controls", Word "the", Word "present", Word "controls", Word "the", Word "past."])]
  , [
    (line2str (str2line text), text)
    ,(line2str [Word "He",Word "who",HypWord "cont",Word "rols"], "He who cont- rols")
    ]
  , [
    (show $ tokLen (Word "He"), "2")
    ,(show $ tokLen (HypWord "cont"), "5")
    ,(show $ tokLen (Blank), "1")
    ]
  , [
    (show $ lineLen [Word "He",Word "who",Word "controls"], "15")
    ,(show $ lineLen [Word "He",Word "who",HypWord "con"], "11")
    ,(show $ lineLen [], "0")
    ]
  , [
    (show $ breakLine 1 [Word "He",Word "who",Word "controls"], show ([]::[Token],[Word "He",Word "who",Word "controls"]))
    ,(show $ breakLine 2 [Word "He",Word "who",Word "controls"], show ([Word "He"],[Word "who",Word "controls"]))
    ,(show $ breakLine 5 [Word "He",Word "who",Word "controls"], show ([Word "He"],[Word "who",Word "controls"]))
    ,(show $ breakLine 6 [Word "He",Word "who",Word "controls"], show ([Word "He",Word "who"],[Word "controls"]))
    ,(show $ breakLine 100 [Word "He",Word "who",Word "controls"], show ([Word "He",Word "who",Word "controls"],[]::[Token]))
    ,(show $ breakLine 0 [], show ([]::[Token],[]::[Token]))
    ]
  , [
    (show $ mergers ["co","nt","ro","ls"], show [("co","ntrols"),("cont","rols"),("contro","ls")])
    ,(show $ mergers ["co","nt"], show [("co","nt")])
    ,(show $ mergers ["co"], "[]")
    ]
  , [
    (show $ hyphenate enHyp (Word "controls"), show [(HypWord "co",Word "ntrols"),(HypWord "cont",Word "rols"),(HypWord "contro",Word "ls")])
    ,(show $ hyphenate enHyp (Word "firefox"), "[]") -- not in the map
    ,(show $ hyphenate enHyp (Word "future."), show [(HypWord "fu",Word "ture."),(HypWord "futu",Word "re.")])
    ,(show $ hyphenate enHyp (Word "future..."), show [(HypWord "fu",Word "ture..."),(HypWord "futu",Word "re...")])
    ]
  , [
    (show $ lineBreaks enHyp 12 [Word "He",Word "who",Word "controls"], show [([Word "He",Word "who"],[Word "controls"]),([Word "He",Word "who",HypWord "co"],[Word "ntrols"]),([Word "He",Word "who",HypWord "cont"],[Word "rols"])])
    ,(show $ lineBreaks enHyp 12 [Word "He"], show [([Word "He"],[]::[Token])])
    ,(show $ lineBreaks enHyp 12 [Word "He",Word "who",Word "controls",Word "the"], show [([Word "He",Word "who"],[Word "controls",Word "the"]),([Word "He",Word "who",HypWord "co"],[Word "ntrols",Word "the"]),([Word "He",Word "who",HypWord "cont"],[Word "rols",Word "the"])])
    ]
  , [
    (show $ insertions 'x' "abcd", show ["xabcd","axbcd","abxcd","abcxd","abcdx"])
    ]
  , [
    (show $ insertBlanks 2 [Word "He",Word "who",Word "controls"], show [[Word "He",Blank,Blank,Word "who",Word "controls"],[Word "He",Blank,Word "who",Blank,Word "controls"],[Word "He",Word "who",Blank,Blank,Word"controls"]])
    ]
  , [
    (show $ blankDistances [Word "He",Blank,Blank,Word "who",Word "controls"], show [1::Integer,0,2])
    ,(show $ (map blankDistances $ insertBlanks 2 [Word "He",Word "who",Word "controls"]), show [[1::Integer,0,2],[1,1,1],[2,0,1]]) --blankDistances [Word "He"] 
    ,(show $ (blankDistances [Word "He"]), "[]")
    ]
  , [
    (show $ var [1,0,2], "0.6666666666666666")
    ,(show $ var [], "0.0")
    ]
  , [
    (show $ lineBadness defaultCosts [Word "He",Word "who",Word "controls"], "0.0")
    ,(show $ lineBadness defaultCosts [Word "He",Blank,Word "who",Word  "controls"], "3.625")
    ,(show $ lineBadness defaultCosts [Word "He",Blank,Word  "who",HypWord "cont"], "4.125")
    ,(show $ lineBadness defaultCosts [Word "He",Blank,Word "who",Blank,Word  "controls"], "6.0")
    ,(show $ lineBadness defaultCosts [Word "He",Blank,Blank,Word "who",Word  "controls"], "6.333333333333333")
    ]
  , [
    (show $ bestLineBreak defaultCosts enHyp 8 [Word "He",Word "who",Word "controls"], show  (Just  ([Word  "He",Blank,Blank,Word  "who"],[Word "controls"])))
    ,(show $ bestLineBreak defaultCosts enHyp 12 [Word "He",Word "who",Word "controls"], show (Just ([Word "He",Word "who",HypWord "cont"],[Word   "rols"])))
    ,(show $ bestLineBreak defaultCosts enHyp 1 [Word "He",Word "who",Word "controls"], "Nothing")
    ]
  , [
    (show $ justifyLine  defaultCosts  enHyp  8  (str2line text), show  [[Word   "He",Blank,Blank,Word   "who"],[Word   "controls"],[Word  "the",Word "past"],[Word  "controls"],[Word "the",Blank,HypWord "fu"],[Word "ture.",Word "He"],[Word "who",Blank,HypWord "co"],[Word
 "ntrols",Word "the",Word "present",Word "controls",Word "the",Word "past."]]) 
    ,(show $ justifyText  defaultCosts  enHyp  8  text , show ["He     who","controls","the past","controls","the   fu-","ture. He","who   co-","ntrols the present controls the past."])
    ]
  ]

test2str :: (Int,[(String,String)]) -> String 
test2str (x,y) = 
  case dropWhile (\(_,(a,b))->a==b) (zip [1::Integer ..] y) of  
    [] -> "Part "++(show x)++" works on given examples."
    ((n,(act,ex)):_) -> "---------------------\nPart "++(show x)++" FAILED\n  Test case "++(show n)++" failed\n  Expected Output: "++ex++"\n    Actual Output: "++act++"\n---------------------"

main :: IO ()
main = putStr $ unlines $ map test2str $ zip [1..] tests

