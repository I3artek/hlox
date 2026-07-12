module Stmts where

import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.State
import Exprs
import Tokens (Token (..))

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  deriving (Show)

consumeSemicolon :: Parser ()
consumeSemicolon =
  do
    do
      consume SEMICOLON
    `catchError` (\s -> throwError $ s ++ " after a value")

statement :: Parser Stmt
statement =
  do
    do
      _ <- match [PRINT]
      printStatement
    `catchError` (\_ -> exprStatement)

printStatement :: Parser Stmt
printStatement =
  do
    do
      e <- expression
      consumeSemicolon
      return $ PrintStmt e

exprStatement :: Parser Stmt
exprStatement = do
  expr <- expression
  consumeSemicolon
  return $ ExprStmt expr

statements :: Parser [Stmt]
statements = do
  isEOF <- peek
  case isEOF of
    EOF -> return []
    _ -> do
      current <- statement
      rest <- statements
      return (current : rest)

parse :: [Token] -> IO [Stmt]
parse ts = do
  let maybeStmts = evalState (runExceptT statements) (ParserState ts)
  case maybeStmts of
    Left err -> do
      print err
      return []
    Right stmts -> return stmts
