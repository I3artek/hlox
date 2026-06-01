module Exprs where

import Control.Monad.State
import Tokens (Token (..))

data Expr
  = Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Unary UnaryExpr

data BinaryExpr = BinaryExpr {binaryOperator :: Token, binaryLeft :: Expr, binaryRight :: Expr}

data GroupingExpr = GroupingExpr {groupedExpression :: Expr}

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

leftAssociativeRecurse :: Parser Expr -> [Token] -> Expr -> Parser Expr
leftAssociativeRecurse lowerPrecedence operators left = do
  maybeOperator <- match operators
  case maybeOperator of
    Just operator -> do
      right <- lowerPrecedence
      let newLeft = Binary $ BinaryExpr operator left right
      leftAssociativeRecurse lowerPrecedence operators newLeft
    Nothing -> return left

leftAssociative :: Parser Expr -> (Expr -> Parser Expr) -> Parser Expr
leftAssociative lowerPrecedence recurseToRight = do
  left <- lowerPrecedence
  recurseToRight left

equalityRecurse :: Expr -> Parser Expr
equalityRecurse = leftAssociativeRecurse comparison [BANG_EQUAL, EQUAL_EQUAL]

equality :: Parser Expr
equality = leftAssociative comparison equalityRecurse

comparisonRecurse :: Expr -> Parser Expr
comparisonRecurse =
  leftAssociativeRecurse
    term
    [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

comparison :: Parser Expr
comparison = leftAssociative term comparisonRecurse

termRecurse :: Expr -> Parser Expr
termRecurse = leftAssociativeRecurse factor [MINUS, PLUS]

term :: Parser Expr
term = leftAssociative factor termRecurse

factorRecurse :: Expr -> Parser Expr
factorRecurse = leftAssociativeRecurse unary [SLASH, STAR]

factor :: Parser Expr
factor = leftAssociative unary factorRecurse

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
