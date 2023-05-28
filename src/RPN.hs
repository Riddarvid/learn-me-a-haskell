module RPN (evalRPN) where
import           Data.Foldable (foldlM)

data Token a = Lit a | Add | Sub | Mul | Div | Ln | Sum | Pow

parseTokens :: Read a => String -> [Token a]
parseTokens = map parseToken . words

parseToken :: Read a => String -> Token a
parseToken str = case str of
  "+"   -> Add
  "-"   -> Sub
  "*"   -> Mul
  "/"   -> Div
  "ln"  -> Ln
  "sum" -> Sum
  "^"   -> Pow
  _     -> Lit (read str)

evalRPN :: (Read a, Floating a) =>String -> Maybe a
evalRPN = evalRPN' . parseTokens

evalRPN' :: (Floating a) =>[Token a] -> Maybe a
evalRPN' tokens = do
  stack <- foldlM applyOp [] tokens
  case stack of
    [x] -> return x
    _   -> Nothing

applyOp :: (Floating a) =>[a] -> Token a -> Maybe [a]
applyOp stack (Lit x)    = Just (x : stack)
applyOp (x : y : xs) Add = Just (x + y : xs)
applyOp (x : y : xs) Sub = Just (y - x : xs)
applyOp (x : y : xs) Mul = Just (x * y : xs)
applyOp (x : y : xs) Div = Just (y / x : xs)
applyOp (x : y : xs) Pow = Just (y ** x : xs)
applyOp (x : xs) Ln      = Just (log x : xs)
applyOp xs Sum           = Just [sum xs]
applyOp _ _              = Nothing
