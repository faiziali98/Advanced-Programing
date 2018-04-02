class YesNo a where -- anything can be treated as boolean will implement this functionality I can call it yesno--
	yesno:: a->Bool -- It only gives type and specific instance gives defination--

instance YesNo Int where -- I can say 0 is false and else is true--
	yesno 0 = False
	yesno _ = True
instance YesNo [a] where
	yesno [] = False
	yesno _ = True 
--instance Num a => YesNo a where
--	yesno 0 = False
--	yesno _ =True

data Mayybe a = Nothin | Justs a
--data Maybe a = Nothing | Just a deriving Ord
--data Maybe a = Just a | Nothing deriving Ord
instance YesNo (Mayybe a) where -- agar haskell class hoti tu is trah likhte 'Num a =>'' 
	yesno Nothin = False 		 -- before this we could not pass maybe to YesNo
	yesno (Justs _) = True

foo :: (YesNo a) => a -> Bool
foo a = yesno a -- I cannot pass other than int because only int is part of yesno
						-- It will also not let me use any other function than yesno because I can add any other type of data in YesNo class
--main1 = putStrLn (show (foo (0 :: Int))) -- 0,10 wagera num type ke hain int me convert karna pare ga
--fool :: (Num a,YesNo a) => a -> a -- Now a can be from both Num and YesNo and the functions of both classes can be used
--main = putStrLn(show 10)
--main1 = (putStrLn.show) 10 
main2 = (putStrLn.show) if (foo [1,2,3]) "Yes" else "No" -- I want to make a class which can be treated as a boolean

--class Functor f where
--	fmap :: (a->b) -> f a -> f b
--instance Functor Maybe where
--	fmap _ Nothing = Nothing
--	fmap k (Just x) = Just (k x)
--instance Functor [] where
-- 	fmap = map --  
--module Main where

--main :: IO ()

--main = putStrLn (show (yesno (12 :: Int)))

--class YesNo a where
--    yesno :: a -> Bool


--instance YesNo Bool where
--    yesno b = b

--instance YesNo [a] where
--    yesno [] = False
--    yesno _ = True


--instance YesNo Int where
--    yesno 0 = False
--    yesno _ = True 