{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe

type Value = Double

data Operation = Sum | Minus | Division | Multiplication | Pow | Root | Concat deriving (Eq, Ord)

data Expressions a = Four | Expression (Expressions a) a (Expressions a)

instance Show Operation where
  show Sum = " + "
  show Minus = " - "
  show Division = " / "
  show Multiplication = " * "
  show Pow = " ^ "
  show Root = " √ "
  show Concat = " ++ "

instance Show (Expressions Operation) where
  show Four = "4"
  show (Expression x op y) = "(" ++ (show x) ++ (show op) ++ (show y) ++ ")"

evalOperation :: Value -> Operation -> Value -> Value
evalOperation x Sum y            = x + y
evalOperation x Minus y          = x - y
evalOperation x Division y       = x / y
evalOperation x Multiplication y = x * y
evalOperation x Pow y            = x ** y
evalOperation x Root y           = x ** (1/y)
evalOperation x Concat y         = read $ (show (floor x)) ++ (show (abs (floor y)))

operations :: [Operation]
operations = [Sum, Minus, Division, Multiplication, Pow, Root, Concat]

possibleExpressions :: Operation -> Operation -> Operation -> [Expressions Operation]
possibleExpressions a b c = [(Expression Four a (Expression Four b (Expression Four c Four))),
                             (Expression (Expression Four b Four) a (Expression Four c Four))]

evalExpression :: Expressions Operation -> Value
evalExpression Four = 4
evalExpression (Expression x op y) = evalOperation (evalExpression x) op (evalExpression y)

findOperationsWithFourResultingIn :: Value -> Maybe (Expressions Operation)
findOperationsWithFourResultingIn x = listToMaybe [ e | a <- operations, b <- operations, c <- operations, e <- (possibleExpressions a b c), x == evalExpression e ]
