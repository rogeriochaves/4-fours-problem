{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe

type Value = Double

data Operation a = NoOp
                 | Sum            (Operation a) (Operation a)
                 | Minus          (Operation a) (Operation a)
                 | Division       (Operation a) (Operation a)
                 | Multiplication (Operation a) (Operation a)
                 | Pow            (Operation a) (Operation a)
                 | Root           (Operation a) (Operation a)
                 | Concat         (Operation a) (Operation a)
                 | Square         (Operation a)
                 | SquareRoot     (Operation a)

singleops = [Square, SquareRoot]
doubleops = [Sum, Minus, Division, Multiplication, Pow, Root, Concat]

type AmountOfFours = Int
type Result = (AmountOfFours, Value)

eval :: Operation Value -> Result
eval NoOp                 = (1, 4)
eval (Sum x y)            = (foursCount [x, y], result x + result y)
eval (Minus x y)          = (foursCount [x, y], result x - result y)
eval (Division x y)       = (foursCount [x, y], result x / result y)
eval (Multiplication x y) = (foursCount [x, y], result x * result y)
eval (Pow x y)            = (foursCount [x, y], result x ** result y)
eval (Root x y)           = (foursCount [x, y], result x ** (1 / (result y)))
eval (Concat x y)         = (foursCount [x, y], read $ (show (floor (result x))) ++ (show (abs (floor (result y)))))
eval (Square x)           = (foursCount [x]   , (result x) ** 2)
eval (SquareRoot x)       = (foursCount [x]   , sqrt (result x))

instance Show (Operation Value) where
  show NoOp                 = "4"
  show (Sum x y)            = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Minus x y)          = "(" ++ (show x) ++ " - " ++ (show y) ++ ")"
  show (Division x y)       = "(" ++ (show x) ++ " / " ++ (show y) ++ ")"
  show (Multiplication x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"
  show (Pow x y)            = "(" ++ (show x) ++ " ^ " ++ (show y) ++ ")"
  show (Root x y)           = "(" ++ (show x) ++ "√ " ++ (show y) ++ ")"
  show (Concat x y)         = "(" ++ (show x) ++ " ++ " ++ (show y) ++ ")"
  show (Square x)           = (show x) ++ "²"
  show (SquareRoot x)       = "√" ++ (show x)

result :: Operation Value -> Value
result = snd . eval

foursCount :: [Operation Value] -> Int
foursCount ops = sum $ fmap (fst . eval) ops

possibleCombinations1 :: [Operation Value]
possibleCombinations1 = [NoOp] ++ [ x NoOp | x <- singleops ]

possibleCombinations2 :: [Operation Value] -> [Operation Value]
possibleCombinations2 []     = [ op x y | op <- doubleops, x <- possibleCombinations1, y <- possibleCombinations1 ]
possibleCombinations2 (x:xs) = [ op x y | op <- doubleops, x <- (possibleCombinations1 ++ possibleCombinations2 xs), y <- (possibleCombinations1 ++ possibleCombinations2 xs) ]

findOperationWithFourResultingIn x = listToMaybe [ op | op <- possibleCombinations2 [NoOp, NoOp], result op == x, foursCount [op] == 4 ]
