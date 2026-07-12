module Stmts where

import Control.Exception (throw)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.State
import Exprs
import Foreign.C (throwErrno)
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
    `catchError` ( \e -> case e of
                     (ConsumeError s) -> throwError $ ConsumeError $ s ++ " after a value"
                     other -> throwError other
                 )

statement :: Parser Stmt
statement =
  do
    do
      _ <- match [PRINT]
      printStatement
    `ifMatchErrorDo` exprStatement

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

varDeclaration :: Parser Stmt
varDeclaration = undefined

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
