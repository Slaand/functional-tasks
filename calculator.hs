
import Control.Applicative
import Control.Monad (forever)
import Data.Maybe (catMaybes)
import Data.List (find)

main::IO()
main = do
  putStrLn "Запишите выражение. Ctrl-C чтобы выйти."
  forever $ do
    line <- getLine
    print $ parse line

binaryTree :: Operator -> Expression -> Expression -> Maybe Double
binaryTree (Operation _ _ op err) a b = do
  aval <- calcTree a
  bval <- calcTree b
  if err aval bval then Nothing else Just $ aval `op` bval
  
calcTree :: Expression -> Maybe Double
calcTree (ExprLiteral val) = Just val
calcTree (ExprPlus a b)  = binaryTree plus   a b
calcTree (ExprMinus a b) = binaryTree minus  a b
calcTree (ExprMult a b)  = binaryTree mult   a b
calcTree (Exprdivs a b)  = binaryTree divs   a b  	

data Expression = 
    ExprLiteral Double 
  | ExprPlus    Expression Expression 
  | ExprMinus   Expression Expression 
  | ExprMult    Expression Expression 
  | Exprdivs    Expression Expression  
  deriving (Show)
  
data Operator = 
    Operation OpName (Expression -> Expression -> Expression) (Double -> Double -> Double) (Double -> Double -> Bool)
  
type OpName = Char

plus, minus, mult, divs :: Operator
plus   = Operation '+' ExprPlus   (+)        (const.const False)
minus  = Operation '-' ExprMinus  (-)        (const.const False)
mult   = Operation '*' ExprMult   (*)        (const.const False)
divs   = Operation '/' Exprdivs   (/)        (const.const False)

opName :: Operator -> OpName
opName (Operation c _ _ _) = c

type Parser a = String -> Maybe (String, a)

safeHead :: [a] -> Maybe a
safeHead s = if null s then Nothing else Just $ head s

next :: Parser Char
next s = do
  h <- safeHead s
  return (tail s, h)
   
oneOf :: [Parser a] -> Parser a
oneOf ps s = case catMaybes $ ps <*> pure s of
  []    -> Nothing
  (h:_) -> Just h 
  
oneOfChar :: String -> Parser Char
oneOfChar ss = oneOf $ parseChar <$> ss

greedy :: Parser a -> Parser [a]
greedy p = greedy' []
  where
    greedy' acc s = case p s of
            Just (s', res) -> greedy' (acc ++ [res]) s'  
            Nothing -> Just (s, acc)

greedy1 :: Parser a -> Parser [a]
greedy1 p s = case greedy p s of
  Nothing -> Nothing
  Just (_, []) -> Nothing
  val -> val
   
parseLiteral :: Parser Expression
parseLiteral s = do
  (s', intPart) <- greedy1 parseDigit s
  case parseChar '.' s' of
    Nothing -> return (s', ExprLiteral $ read intPart) 
    Just (s'', _) -> do
      (s''', fracPart) <- greedy parseDigit s''
      return (s''', ExprLiteral $ read $ intPart ++ "." ++ fracPart)

parseParent :: Parser Expression
parseParent s = do
  (s', _) <- parseChar '(' s
  (s'', expr) <- parseExpr s'
  (s''', _) <- parseChar ')' s''
  return (s''', expr)
  
parseTerm :: Parser Expression
parseTerm = oneOf [parseLiteral, parseParent]

parseExpr :: Parser Expression
parseExpr = byLayer [[plus, minus], [mult, divs]] parseTerm

parseInTree :: String -> Maybe Expression
parseInTree s = case parseExpr s of
  Just ([], tree) -> Just tree
  _ -> Nothing

parseChar ::  Char -> Parser Char
parseChar c s = do
  (s', rc) <- next s
  if rc == c then Just (s', c) else Nothing

parseDigit :: Parser Char
parseDigit = oneOfChar "1234567890"
  
parse :: String -> Maybe Double
parse s = calcTree =<< parseInTree (filter (/=' ') s)

byLayer :: [[Operator]] -> Parser Expression -> Parser Expression
byLayer [] l s = l s
byLayer (ops:opss) l s = do
  (s', res) <- nextLayer s
  (s'', resTail) <- greedy layerTail s'
  return (s'', toExpr $ (head ops, res):resTail)  
  where
    opExpr (Operation _ f _ _) = f
    
    nextLayer = byLayer opss l
    layerTail ts = do
      (s', opChar) <- oneOfChar (opName <$> ops) ts
      (s'', resTail) <- nextLayer s'
      op <- find ((== opChar).opName) ops
      return (s'', (op, resTail))
   
    toExpr [] = undefined
    toExpr [(_, e)] = e
    toExpr ((_, e1):(op, e2):es) = toExpr $ (op, opExpr op e1 e2) : es