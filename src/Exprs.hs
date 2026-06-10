module Exprs where

import Control.Monad.State
import Tokens (Token (..))

data Expr
  = Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Unary UnaryExpr

data Value
  = LoxString String
  | LoxNumber Double
  | LoxObject String -- for now
  | LoxBool Bool
  | LoxNil
  deriving (Show, Eq, Ord)

data BinaryExpr = BinaryExpr {binaryOperator :: Token, binaryLeft :: Expr, binaryRight :: Expr}

data GroupingExpr = GroupingExpr {groupedExpression :: Expr}

data LiteralExpr = LiteralExpr {value :: Token}

data UnaryExpr = UnaryExpr {unaryOperator :: Token, unaryRight :: Expr}

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

-- TODO: move evaluation to a different file

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

instance Show Expr where
  show (Binary e) =
    "("
      ++ show (binaryOperator e)
      ++ " "
      ++ show (binaryLeft e)
      ++ " "
      ++ show (binaryRight e)
      ++ ")"
  show (Grouping e) = "(group " ++ show (groupedExpression e) ++ ")"
  show (Literal e) = "(literal: " ++ show (value e) ++ ")"
  show (Unary e) =
    "("
      ++ show (unaryOperator e)
      ++ " "
      ++ show (unaryRight e)
      ++ ")"

data ParserState = ParserState {tokens :: [Token]}

type Parser a = State ParserState a

peek :: Parser Token
peek = do
  ps <- get
  let ts = tokens ps
  case ts of
    [] -> return EOF
    (t : _) -> return t

advance :: Parser ()
advance = do
  ps <- get
  let ts = tokens ps
  case ts of
    [] -> return ()
    (_ : rest) -> put ps {tokens = rest}

match :: [Token] -> Parser (Maybe Token)
match expected = do
  next <- peek
  if next `elem` expected
    then do
      advance
      return $ Just next
    else return Nothing

expression :: Parser Expr
expression = do
  equality

leftAssociative :: [Token] -> Parser Expr -> Parser Expr
leftAssociative operators lowerPrecedence = do
  left <- lowerPrecedence
  recurseToRight left
  where
    recurseToRight left = do
      maybeOperator <- match operators
      case maybeOperator of
        Just operator -> do
          right <- lowerPrecedence
          let newLeft = Binary $ BinaryExpr operator left right
          recurseToRight newLeft
        Nothing -> return left

equality :: Parser Expr
equality = leftAssociative [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: Parser Expr
comparison = leftAssociative [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: Parser Expr
term = leftAssociative [MINUS, PLUS] factor

factor :: Parser Expr
factor = leftAssociative [SLASH, STAR] unary

unary :: Parser Expr
unary = do
  maybeOperator <- match [BANG, MINUS]
  case maybeOperator of
    Just operator -> do
      right <- unary
      return $ Unary $ UnaryExpr operator right
    Nothing -> primary

primary :: Parser Expr
primary = do
  next <- peek
  advance
  case next of
    FALSE -> return $ Literal $ LiteralExpr FALSE
    TRUE -> return $ Literal $ LiteralExpr TRUE
    NIL -> return $ Literal $ LiteralExpr NIL
    NUMBER x -> return $ Literal $ LiteralExpr $ NUMBER x
    STRING s -> return $ Literal $ LiteralExpr $ STRING s
    LEFT_PAREN -> do
      e <- expression
      closing <- peek
      case closing of
        RIGHT_PAREN -> do
          advance
          return $ Grouping $ GroupingExpr e
        _ -> do advance; error "Closing parentheses missing" -- This should raise an error
    _ -> error $ "Unexpected token: " ++ show next

parse :: [Token] -> Expr
parse ts = evalState expression (ParserState ts)
