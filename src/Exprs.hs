module Exprs where

import Control.Monad.Except (ExceptT, catchError, runExceptT, throwError)
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

data ParseError = MatchError | ConsumeError String | InputError String deriving Show

type Parser a = ExceptT ParseError (State ParserState) a


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


match :: [Token] -> Parser Token
match expected = do
  next <- peek
  if next `elem` expected
    then do
      advance
      return next
    else throwError MatchError

ifMatchErrorDo :: Parser a -> Parser a -> Parser a
ifMatchErrorDo action errorCase =
  catchError action $ \e -> do
    case e of
      MatchError -> errorCase
      err -> throwError err


consume :: Token -> Parser ()
consume expected = do
  next <- peek
  if next == expected
    then do
      advance
      return ()
    else throwError $ ConsumeError $ "Expected " ++ show expected

expression :: Parser Expr
expression = do
  equality

leftAssociative :: [Token] -> Parser Expr -> Parser Expr
leftAssociative operators lowerPrecedence = do
  left <- lowerPrecedence
  recurseToRight left
  where
    recurseToRight left =
      do
        do
          operator <- match operators
          right <- lowerPrecedence
          let newLeft = Binary $ BinaryExpr operator left right
          recurseToRight newLeft
        `catchError` (\_ -> return left)

equality :: Parser Expr
equality = leftAssociative [BANG_EQUAL, EQUAL_EQUAL] comparison

comparison :: Parser Expr
comparison = leftAssociative [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term

term :: Parser Expr
term = leftAssociative [MINUS, PLUS] factor

factor :: Parser Expr
factor = leftAssociative [SLASH, STAR] unary

unary :: Parser Expr
unary =
  do
    do
      operator <- match [BANG, MINUS]
      right <- unary
      return $ Unary $ UnaryExpr operator right
    `catchError` (\_ -> primary)

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
        _ -> do advance; throwError $ InputError $ "Closing parentheses missing" -- This should raise an error
    _ -> throwError $ InputError $ "Unexpected token: " ++ show next
