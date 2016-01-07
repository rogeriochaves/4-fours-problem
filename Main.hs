import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>))
import System.Environment (getArgs)

type Value = Double

data Operation a = Four
                 | Sum            (Operation a) (Operation a)
                 | Minus          (Operation a) (Operation a)
                 | Division       (Operation a) (Operation a)
                 | Multiplication (Operation a) (Operation a)
                 | Pow            (Operation a) (Operation a)
                 | Root           (Operation a) (Operation a)
                 | Factorial      (Operation a)
                 | SquareRoot     (Operation a)
                 | Termial        (Operation a)

noops = [Four]
singleops = [Factorial, SquareRoot, Termial]
doubleops = [Sum, Minus, Division, Multiplication, Pow, Root]

type AmountOfFours = Int
type Result = (AmountOfFours, Value)

eval :: Operation Value -> Result
eval Four                 = (1, 4)
eval (Sum x y)            = (foursCount [x, y], result x + result y)
eval (Minus x y)          = (foursCount [x, y], result x - result y)
eval (Division x y)       = (foursCount [x, y], result x / result y)
eval (Multiplication x y) = (foursCount [x, y], result x * result y)
eval (Pow x y)            = (foursCount [x, y], result x ** result y)
eval (Root x y)           = (foursCount [x, y], result x ** (1 / (result y)))
eval (Factorial x)        = (foursCount [x]   , product [1..(result x)])
eval (SquareRoot x)       = (foursCount [x]   , sqrt (result x))
eval (Termial x)          = (foursCount [x]   , sum [1..(result x)])

instance Show (Operation a) where
  show Four                 = "4"
  show (Sum x y)            = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Minus x y)          = "(" ++ (show x) ++ " - " ++ (show y) ++ ")"
  show (Division x y)       = "(" ++ (show x) ++ " / " ++ (show y) ++ ")"
  show (Multiplication x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Pow x y)            = "(" ++ (show x) ++ " ^ " ++ (show y) ++ ")"
  show (Root x y)           = "(" ++ (show x) ++ "√ " ++ (show y) ++ ")"
  show (Factorial x)        = (show x) ++ "!"
  show (SquareRoot x)       = "√" ++ (show x)
  show (Termial x)          = (show x) ++ "?"

result :: Operation Value -> Value
result = snd . eval

foursCount :: [Operation Value] -> Int
foursCount ops = sum ((fst . eval) <$> ops)

possibleCombinations1 :: [Operation Value]
possibleCombinations1 = noops ++ [ x y | x <- singleops, y <- noops ]

possibleCombinations2 :: Int -> [Operation Value]
possibleCombinations2 0      = [ op x y | op <- doubleops, x <- possibleCombinations1, y <- possibleCombinations1 ]
possibleCombinations2 opsnum = [ op x y | op <- doubleops, x <- (possibleCombinations1 ++ possibleCombinations2 (opsnum - 1)), y <- (possibleCombinations1 ++ possibleCombinations2 (opsnum - 1)) ]

findOperationWithFourResultingIn x = listToMaybe [ op | op <- possibleCombinations2 1, result op == x, foursCount [op] == 4 ]

main = do
  number <- getArgs
  print $ findOperationWithFourResultingIn $ read $ head number
