import Data.Maybe (listToMaybe)
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

eval :: Operation Value -> Value
eval Four                 = 4
eval (Sum x y)            = eval x + eval y
eval (Minus x y)          = eval x - eval y
eval (Division x y)       = eval x / eval y
eval (Multiplication x y) = eval x * eval y
eval (Pow x y)            = eval x ** eval y
eval (Root x y)           = eval x ** (1 / eval y)
eval (Factorial x)        = product [1..eval x]
eval (SquareRoot x)       = sqrt (eval x)
eval (Termial x)          = sum [1..eval x]

foursCount :: Operation Value -> Int
foursCount Four                 = 1
foursCount (Sum x y)            = foursCount x + foursCount y
foursCount (Minus x y)          = foursCount x + foursCount y
foursCount (Division x y)       = foursCount x + foursCount y
foursCount (Multiplication x y) = foursCount x + foursCount y
foursCount (Pow x y)            = foursCount x + foursCount y
foursCount (Root x y)           = foursCount x + foursCount y
foursCount (Factorial x)        = foursCount x
foursCount (SquareRoot x)       = foursCount x
foursCount (Termial x)          = foursCount x

instance Show (Operation a) where
  show Four                 = "4"
  show (Sum x y)            = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Minus x y)          = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Division x y)       = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Multiplication x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Pow x y)            = "(" ++ show x ++ " ^ " ++ show y ++ ")"
  show (Root x y)           = "(" ++ show x ++ "√ " ++ show y ++ ")"
  show (Factorial x)        = show x ++ "!"
  show (SquareRoot x)       = "√" ++ show x
  show (Termial x)          = show x ++ "?"

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
