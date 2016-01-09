import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import Control.Applicative ((<$>))

type Value = Double

data Operation a = NoOp     NoOpOperator
                 | SingleOp SingleOperator (Operation a)
                 | DoubleOp DoubleOperator (Operation a) (Operation a)

data NoOpOperator   = Four
data SingleOperator = Factorial | SquareRoot | Termial
data DoubleOperator = Sum | Minus | Division | Multiplication | Pow | Root

eval :: Operation Value -> Value
eval (NoOp Four)       = 4
eval (SingleOp op x)   = evalSingleOp op x
eval (DoubleOp op x y) = evalDoubleOp op x y

evalSingleOp :: SingleOperator -> Operation Value -> Value
evalSingleOp Factorial  x = product [1..eval x]
evalSingleOp SquareRoot x = sqrt (eval x)
evalSingleOp Termial    x = sum [1..eval x]

evalDoubleOp :: DoubleOperator -> Operation Value -> Operation Value -> Value
evalDoubleOp Sum x y            = eval x + eval y
evalDoubleOp Minus x y          = eval x - eval y
evalDoubleOp Division x y       = eval x / eval y
evalDoubleOp Multiplication x y = eval x * eval y
evalDoubleOp Pow x y            = eval x ** eval y
evalDoubleOp Root x y           = eval x ** (1 / eval y)

foursCount :: Operation Value -> Int
foursCount (NoOp _)         = 1
foursCount (SingleOp _ x)   = foursCount x
foursCount (DoubleOp _ x y) = foursCount x + foursCount y

instance Show (Operation a) where
  show (NoOp op)               = show op
  show (SingleOp SquareRoot x) = show SquareRoot ++ show x
  show (SingleOp op x)         = show x ++ show op
  show (DoubleOp op x y)       = "(" ++ show x ++ " " ++ show op ++ " " ++ show y ++ ")"

instance Show NoOpOperator where
  show Four = "4"

instance Show SingleOperator where
  show Factorial = "!"
  show SquareRoot = "√"
  show Termial = "?"

instance Show DoubleOperator where
  show Sum = "+"
  show Minus = "-"
  show Division = "/"
  show Multiplication = "*"
  show Pow = "^"
  show Root = "√"

noops     = NoOp     <$> [Four]
singleops = SingleOp <$> [Factorial, SquareRoot, Termial]
doubleops = DoubleOp <$> [Sum, Minus, Division, Multiplication, Pow, Root]

possibleCombinations1 :: [Operation Value]
possibleCombinations1 = noops ++ [ x y | x <- singleops, y <- noops ]

possibleCombinations2 :: Int -> [Operation Value]
possibleCombinations2 0      = [ op x y | op <- doubleops,
                                          x <- possibleCombinations1, y <- possibleCombinations1 ]
possibleCombinations2 opsnum = [ op x y | op <- doubleops,
                                          x <- possibleCombinations1 ++ possibleCombinations2 (opsnum - 1),
                                          y <- possibleCombinations1 ++ possibleCombinations2 (opsnum - 1) ]

findOperationWithFourResultingIn :: Value -> Maybe (Operation Value)
findOperationWithFourResultingIn x = listToMaybe [ op | op <- possibleCombinations2 1,
                                                        eval op == x,
                                                        foursCount op == 4 ]

main = do
  number <- getArgs
  print $ findOperationWithFourResultingIn $ read $ head number
