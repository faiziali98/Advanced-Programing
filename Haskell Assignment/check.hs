import Data.List
import System.IO


--summ :: Int -> Int -> Int
--summ a b = a+b
--pro :: Int -> Int -> Int
--pro a b = a*b
 
new = map (summ . pro) [1,3]
	where
		summ a = a+2
		pro a = a*2

k :: Int -> Int -> Bool
k a b
	| a == b = True
	| otherwise = False

--eqal :: [Int] -> Int
--eqal (x:y:z) = z
