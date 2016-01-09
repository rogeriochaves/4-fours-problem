{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.List (transpose)

type Value = Double

data Operation a = NoOp     NoOpOperator
                 | SingleOp SingleOperator (Operation a)
                 | DoubleOp DoubleOperator (Operation a) (Operation a)

data NoOpOperator   = Four
data SingleOperator = Factorial | SquareRoot | Termial
data DoubleOperator = Sum | Minus | Division | Multiplication | Pow | Root

data OperationResult a = Result a (Maybe (Operation a))

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

instance Show (OperationResult Value) where
  show (Result x Nothing)  = show (floor x :: Int) ++ " = Nothing"
  show (Result x (Just r)) = show (floor x :: Int) ++ " = " ++ show r

noops     = NoOp     <$> [Four]
singleops = SingleOp <$> [Termial, SquareRoot, Factorial]
doubleops = DoubleOp <$> [Sum, Minus, Multiplication, Division, Pow, Root]

possibleCombinations1 :: Int -> [Operation Value]
possibleCombinations1 0 = noops ++ [ x y | x <- singleops, y <- noops ]
possibleCombinations1 1 = noops ++ [ x y | x <- singleops, y <- possibleCombinations1 0 ]

-- this combines operations like this ((4 + 4) + 4) + 4)
possibleCombinations2 :: Int -> [Operation Value]
possibleCombinations2 0 = [ op x y | op <- doubleops,
                                      x <- possibleCombinations1 1, y <- possibleCombinations1 0 ]
possibleCombinations2 1 = [ op x y | op <- doubleops,
                                      x <- possibleCombinations2 0,
                                      y <- possibleCombinations1 1 ]
possibleCombinations2 2 = [ op x y | op <- doubleops,
                                      x <- possibleCombinations2 1,
                                      y <- possibleCombinations1 1 ]

-- this combines operations like this ((4 + 4) + (4 + 4))
possibleCombinations2' :: Int -> [Operation Value]
possibleCombinations2' 0 = [ op x y | op <- doubleops,
                                       x <- possibleCombinations1 1, y <- possibleCombinations1 0 ]
possibleCombinations2' 1 = [ op x y | op <- doubleops,
                                       x <- possibleCombinations2 0,
                                       y <- possibleCombinations2 0 ]

lazyMerge :: [[a]] -> [a]
lazyMerge = concat . transpose

findOperationWithFourResultingIn :: Value -> OperationResult Value
findOperationWithFourResultingIn x = Result x $ listToMaybe [ op | op <- lazyMerge [possibleCombinations2 2, possibleCombinations2' 1],
                                                                   eval op == x,
                                                                   foursCount op == 4 ]

main = do
  number <- getArgs
  if length number >= 2 then
    mapM_ print $ findOperationWithFourResultingIn <$> [read (head number)..read (last number)]
  else
    print $ findOperationWithFourResultingIn $ read $ head number
