module Exprs where

import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.State
import Tokens (Token (..))

data Expr
  = Assignment AssignmentExpr
  | Binary BinaryExpr
  | Grouping GroupingExpr
  | Literal LiteralExpr
  | Logical LogicalExpr
  | Unary UnaryExpr
  | Variable VariableExpr

data AssignmentExpr = AssignmentExpr {assignmentName :: String, assignmentValue :: Expr}

data BinaryExpr = BinaryExpr {binaryOperator :: Token, binaryLeft :: Expr, binaryRight :: Expr}

data GroupingExpr = GroupingExpr {groupedExpression :: Expr}

data LiteralExpr = LiteralExpr {value :: Token}

data LogicalExpr = LogicalExpr {logicalOperator :: Token, logicalLeft :: Expr, logicalRight :: Expr}

data UnaryExpr = UnaryExpr {unaryOperator :: Token, unaryRight :: Expr}

data VariableExpr = VariableExpr {varName :: String}

instance Show Expr where
  show (Assignment e) = "(" ++ assignmentName e ++ " = " ++ show (assignmentValue e) ++ ")"
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
  show (Logical e) =
    "("
      ++ show (logicalOperator e)
      ++ " "
      ++ show (logicalLeft e)
      ++ " "
      ++ show (logicalRight e)
      ++ ")"
  show (Unary e) =
    "("
      ++ show (unaryOperator e)
      ++ " "
      ++ show (unaryRight e)
      ++ ")"
  show (Variable e) = "(var: " ++ varName e ++ ")"

data ParserState = ParserState {tokens :: [Token]}

data ParseError
  = MatchError
  | ConsumeError String
  | InputError String
  | AssignmentError String
  deriving (Show)

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

matchAnyIdentifier :: Parser String
matchAnyIdentifier = do
  next <- peek
  case next of
    IDENTIFIER name -> do advance; return name
    _ -> throwError MatchError

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
  assignment

assignment :: Parser Expr
assignment = do
  expr <- logicalOr
  do
    _ <- match [EQUAL]
    right <- assignment
    case expr of
      (Variable (VariableExpr name)) -> return $ Assignment $ AssignmentExpr name right
      _ -> throwError $ AssignmentError $ "'" ++ show expr ++ "' is not a valid lvalue!"
    `ifMatchErrorDo` return expr

logicalOp :: Token -> Parser Expr -> Parser Expr
logicalOp operator lowerPrecedence = do
  left <- lowerPrecedence
  recurseToRight left
  where
    recurseToRight left =
      do
        do
          _ <- match [operator]
          right <- lowerPrecedence
          let newLeft = Logical $ LogicalExpr operator left right
          recurseToRight newLeft
        `ifMatchErrorDo` return left

logicalOr :: Parser Expr
logicalOr = logicalOp OR logicalAnd

logicalAnd :: Parser Expr
logicalAnd = logicalOp AND equality

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
    IDENTIFIER s -> return $ Variable $ VariableExpr s
    LEFT_PAREN -> do
      e <- expression
      closing <- peek
      case closing of
        RIGHT_PAREN -> do
          advance
          return $ Grouping $ GroupingExpr e
        _ -> do advance; throwError $ InputError $ "Closing parentheses missing" -- This should raise an error
    _ -> throwError $ InputError $ "Unexpected token: " ++ show next
