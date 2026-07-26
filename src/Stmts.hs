module Stmts where

import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.State
import Exprs
import Tokens (Token (..))

data Stmt
  = ExprStmt Expr
  | IfStmt Expr Stmt Stmt
  | PrintStmt Expr
  | VarStmt String Expr
  | BlockStmt [Stmt]
  | NOPStmt
  deriving (Show)

consumeSemicolon :: Parser ()
consumeSemicolon =
  do
    do
      consume SEMICOLON
    `catchError` ( \e -> case e of
                     (ConsumeError s) -> throwError $ ConsumeError $ s ++ " after a value"
                     other -> throwError other
                 )

statement :: Parser Stmt
statement =
  do
    do
      next <- match [IF, PRINT, LEFT_BRACE]
      case next of
        IF -> ifStatement
        PRINT -> printStatement
        LEFT_BRACE -> blockStatement
        _ -> undefined
    `ifMatchErrorDo` exprStatement

ifStatement :: Parser Stmt
ifStatement = do
  consume LEFT_PAREN
  cond <- expression
  consume RIGHT_PAREN
  thenB <- statement
  do
    _ <- match [ELSE]
    elseB <- statement
    return (IfStmt cond thenB elseB)
    `ifMatchErrorDo` do return $ IfStmt cond thenB NOPStmt

blockStatement :: Parser Stmt
blockStatement = do
  stmts <- block
  consume RIGHT_BRACE
  return $ BlockStmt stmts

-- This is almost the same as program, but we keep them separate on purpose
-- as we don't want program to stop parsing on a random "}"
block :: Parser [Stmt]
block = do
  next <- peek
  case next of
    EOF -> return []
    RIGHT_BRACE -> return []
    _ -> do
      current <- declaration
      rest <- block
      return (current : rest)

printStatement :: Parser Stmt
printStatement =
  do
    e <- expression
    consumeSemicolon
    return $ PrintStmt e

exprStatement :: Parser Stmt
exprStatement = do
  expr <- expression
  consumeSemicolon
  return $ ExprStmt expr

varDeclaration :: Parser Stmt
varDeclaration = do
  name <- matchAnyIdentifier
  do
    do
      _ <- match [EQUAL]
      initializer <- expression
      consumeSemicolon
      return $ VarStmt name initializer
    `ifMatchErrorDo` do
      consumeSemicolon
      return $ VarStmt name $ Literal $ LiteralExpr NIL

declaration :: Parser Stmt
declaration =
  do
    do
      decl <- match [VAR]
      case decl of
        VAR -> varDeclaration
        _ -> throwError $ InputError "Not supported"
    `ifMatchErrorDo` statement

program :: Parser [Stmt]
program = do
  isEOF <- peek
  case isEOF of
    EOF -> return []
    _ -> do
      current <- declaration
      rest <- program
      return (current : rest)

parse :: [Token] -> IO [Stmt]
parse ts = do
  let maybeStmts = evalState (runExceptT program) (ParserState ts)
  case maybeStmts of
    Left err -> do
      print err
      return []
    Right stmts -> return stmts
