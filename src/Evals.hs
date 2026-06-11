module Evals where

import Exprs (BinaryExpr (..), Expr (..), GroupingExpr (..), LiteralExpr (..), UnaryExpr (..))
import Tokens (Token (..))

data Value
  = LoxString String
  | LoxNumber Double
  | LoxObject String -- for now
  | LoxBool Bool
  | LoxNil
  deriving (Show, Eq, Ord)

evalExpr :: Expr -> Value
evalExpr (Binary e) = evalBinaryExpr left op right
  where
    left = evalExpr $ binaryLeft e
    right = evalExpr $ binaryRight e
    op = binaryOperator e
evalExpr (Grouping e) = evalExpr $ groupedExpression e
evalExpr (Literal e) = evalLiteral $ value e
evalExpr (Unary e) = evalUnaryExpr op right
  where
    right = evalExpr $ unaryRight e
    op = unaryOperator e

evalBinaryExpr :: Value -> Token -> Value -> Value
evalBinaryExpr (LoxString a) PLUS (LoxString b) = LoxString (a ++ b)
evalBinaryExpr (LoxNumber a) PLUS (LoxNumber b) = LoxNumber (a + b)
evalBinaryExpr (LoxNumber a) MINUS (LoxNumber b) = LoxNumber (a - b)
evalBinaryExpr (LoxNumber _) SLASH (LoxNumber 0) = error "Division by zero not allowed!"
evalBinaryExpr (LoxNumber a) SLASH (LoxNumber b) = LoxNumber (a / b)
evalBinaryExpr (LoxNumber a) STAR (LoxNumber b) = LoxNumber (a * b)
evalBinaryExpr v1 BANG_EQUAL v2 = LoxBool $ v1 /= v2
evalBinaryExpr v1 EQUAL_EQUAL v2 = LoxBool $ v1 == v2
evalBinaryExpr v1 GREATER v2 = LoxBool $ v1 > v2
evalBinaryExpr v1 GREATER_EQUAL v2 = LoxBool $ v1 >= v2
evalBinaryExpr v1 LESS v2 = LoxBool $ v1 < v2
evalBinaryExpr v1 LESS_EQUAL v2 = LoxBool $ v1 <= v2
evalBinaryExpr t1 op t2 = error $ "Not possible to perform " ++ show t1 ++ " " ++ show op ++ " " ++ show t2

evalLiteral :: Token -> Value
evalLiteral (STRING s) = LoxString s
evalLiteral (NUMBER n) = LoxNumber n
evalLiteral (IDENTIFIER x) = LoxObject x
evalLiteral FALSE = LoxBool False
evalLiteral TRUE = LoxBool True
evalLiteral _ = LoxNil

evalUnaryExpr :: Token -> Value -> Value
evalUnaryExpr MINUS (LoxNumber b) = LoxNumber (-b)
evalUnaryExpr BANG t = LoxBool $ not $ isTruthy t
evalUnaryExpr op t = error $ "Not possible to perform " ++ show op ++ " " ++ show t

isTruthy :: Value -> Bool
isTruthy (LoxBool False) = False
isTruthy LoxNil = False
isTruthy _ = True
