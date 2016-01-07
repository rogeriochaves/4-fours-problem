import Data.Maybe

type Value = Double

data Operation = Sum | Minus | Division | Multiplication deriving (Eq, Ord)

data Expressions a = Four | Expression (Expressions a) a (Expressions a) deriving (Show)

instance Show Operation where
  show Sum = "+"
  show Minus = "-"
  show Division = "/"
  show Multiplication = "*"

evalOperation :: Value -> Operation -> Value -> Value
evalOperation x Sum y            = x + y
evalOperation x Minus y          = x - y
evalOperation x Division y       = x / y
evalOperation x Multiplication y = x * y

operations :: [Operation]
operations = [Sum, Minus, Division, Multiplication]

possibleExpressions :: Operation -> Operation -> Operation -> [Expressions Operation]
possibleExpressions a b c = [(Expression Four a (Expression Four b (Expression Four c Four))),
                             (Expression (Expression Four b Four) a (Expression Four c Four))]

evalExpression :: Expressions Operation -> Value
evalExpression Four = 4
evalExpression (Expression x op y) = evalOperation (evalExpression x) op (evalExpression y)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

findOperationsWithFourResultingIn :: Value -> Maybe (Expressions Operation)
findOperationsWithFourResultingIn x = head' [ e | a <- operations, b <- operations, c <- operations, e <- (possibleExpressions a b c), x == evalExpression e ]
