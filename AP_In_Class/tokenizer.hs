import Data.Char

data Operator = Plus | Times deriving(Show,Eq)
data Token = TokNum Int | TokOp Operator deriving (Show,Eq)

operator::Char -> Operator
operator '+' = Plus
operator '*' = Times

alnums :: (Char->Bool) -> String -> (String,String)
--alnums :: String -> (String,String)
alnums str = als "" str
	where
		als acc [] = (acc, [])
		als acc (c:cs) | isDigit c = 
				let(acc', cs') = als acc cs in
				(c:acc', cs')
			| otherwise = (acc, c:cs)

tokenize :: String -> [Token]
tokenize [] = []

--tokenize(c:cs)
--		| elem c "+*" = TokOp(Operator c) : tokenize cs
--		| isDigit c = TokNum(digitToInt c) : tokenize cs
--		| isSpace c = tokenize cs
--		| otherwise = error "Can not tokenize"
tokenize(c:cs)
		| elem c "+*" = TokOp(Operator c) : tokenize cs
		| isDigit c = TokNum(digitToInt c) : tokenize cs
		| isSpace c = tokenize cs
		| otherwise = error "Can not tokenize"

parse :: [Token] -> Tree
parse = undefined

evaluate :: Tree -> Double
evaluate = undefined




--Sir wala

import Data.Char

data Parser a = Parser ([Token] -> [(a, [Token])])
instance Functor Parser where fmap = undefined
instance Applicative Parser where pure = undefined; (<*>) = undefined
instance Monad Parser where
  return x = Parser (\toks -> [(x, toks)])
  (Parser leftParser) >>= getRightParser = Parser (\toks ->
    [(rightParseResult, toksAfterRight)
      | (leftParseResult, toksAfterLeft) <- leftParser toks
      , (rightParseResult, toksAfterRight) <-
          let (Parser rightParser) = getRightParser leftParseResult in
          rightParser toksAfterLeft])

(Parser leftParser) <|> (Parser rightParser)
  = Parser (\toks -> leftParser toks ++ (rightParser toks))

data Tree = SumNode Tree Tree | ProdNode Tree Tree | NumNode Double deriving Show
data Token = TokNum Double | TokPlus | TokTimes deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize ('+':cs) = TokPlus : tokenize cs
tokenize ('*':cs) = TokTimes : tokenize cs
tokenize (c:cs)
  | isDigit c =
      let (digs, cs') = span isDigit cs in -- Digs idhar calculate ho ra he neeche nai
      TokNum (read (c:digs)) : tokenize cs' -- Concatenate here
  | isSpace c = tokenize cs
  | otherwise = error "Cannot tokenize"

parse :: [Token] -> Tree
parse toks = let (Parser f) = expr in
  fst $ head $ filter ((==[]).snd) (f toks)

isTok :: Token -> Parser Token
isTok tok = Parser isTok'
  where isTok' (t:rest) | t==tok = [(t, rest)]
        isTok' _ = []

isNum :: Parser Double
isNum = Parser isNum'
  where isNum' (TokNum num:rest) = [(num, rest)]
        isNum' _ = []

term :: Parser Tree
term =
  do
    num <- isNum
    return $ NumNode num
  <|>
  do
    num <- isNum
    isTok TokTimes
    termTree <- term
    return $ ProdNode (NumNode num) termTree

expr :: Parser Tree
expr =
  term
  <|>
  do
    termTree <- term
    isTok TokPlus
    exTree <- expr
    return $ SumNode termTree exTree

eval :: Tree -> Double
eval (NumNode num) = num
eval (SumNode left right) = eval left + (eval right)
eval (ProdNode left right) = eval left * (eval right)

main :: IO ()
main = (print.eval.parse.tokenize) "10*10 + 20*30"
