module Exprs where

import Tokens

data Expr
  = Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Unary UnaryExpr

data BinaryExpr = BinaryExpr {binaryOperator :: Token, binaryLeft :: Expr, binaryRight :: Expr}

data GroupingExpr = GroupingExpr {expression :: Expr}

data LiteralExpr = LiteralExpr {value :: Token}

data UnaryExpr = UnaryExpr {unaryOperator :: Token, unaryRight :: Expr}

instance Show Expr where
  show (Binary e) =
    "("
      ++ show (binaryOperator e)
      ++ " "
      ++ show (binaryLeft e)
      ++ " "
      ++ show (binaryRight e)
      ++ ")"
  show (Grouping e) = "(group " ++ show (expression e) ++ ")"
  show (Literal e) = show (value e)
  show (Unary e) =
    "("
      ++ show (unaryOperator e)
      ++ " "
      ++ show (unaryRight e)
      ++ ")"
