import Data.Char

data Tree = SumNode Tree Tree | ProdNode Tree Tree
	| NumNode Double deriving Show

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':cs) = TokPlus : tokenize cs
tokenize ('*':cs) = TokTimes : tokenize cs
tokenize (c:cs)
	| isDigit c = 
			let (digs, cs') = span isDigit cs in
			TokNum (read (c:digs)) : tokenize cs'
	| isSpace c = tokenize cs
	| otherwise = error "Cannot tokenize"

parse :: [Token] -> (Tree,[Token]) -- [Token] is for remaining token list
parse = error "Missing parser"

eval :: Tree -> Double
eval (NumNode x) = x
eval (SumNode l r) = (eval l) + (eval r)
eval (ProdNode l r) = (eval l) + (eval r)

parse :: [Token] -> Tree
parse token = 

expr :: [Token] -> Tree
expr toks = 
	case term toks of 
		(l, TokPlus:ts) -> let (r,ts') = expr ts in (SumNode l r, ts')
		x -> x

term :: [Token] -> (Tree, [Token])
term ((TokNum x):TokTimes:ts) = 
	let (r, ts') = term ts .... (ProdNode (NumNode x) r, ts')
term ((TokNum x):ts) = (NumNode x, ts)
term _ = error "Parser Error"


main :: IO() 
main = (print.eval.parse.tokenize) "10*10 + 20*30"
	-->> (print.eval)(SumNode
	--	(ProdNode (NumNode 10)(NumNode 10))
	--	(ProdNode (NumNode 20)(NumNode 30)))
	-->> (print.eval.parse.tokenize) "10*10 + 20*30"

data Parser a = Parser ([Token] -> [(a,[Token])])

instance Functor Parser where
	fmap = undefined
instance Applicative Parser where
 	pure = undefined
 	(<*>) = undefined
 instance Monad Parser where
  	return a = Parser (\x -> [(a,x)])
  	(Parser f1) >>= f2 = Parser (\v -> 
  		[(t,toks')
  			| (x,toks) <- f1 v 
  			, (t,toks') <- ... f2 x... --box and a function that gives box and a bigger box that determines what to do with box   

isTok :: Token -> Parser Token
isTok tok = Parser isTok'
	where isTok' (c:cs) | c==tok = [(tok,cs)]
		  isTok' _ = []

-- isNum' takes an expression and returns tokenized thing
isNum :: Parser Double
isNum = Parser isNum'
	where isNum' = (TokNum n:cs) = [(n, cs)]
		  isNum' _ = []