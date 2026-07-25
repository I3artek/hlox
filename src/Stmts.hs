module Stmts where

import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.State
import Exprs
import Tokens (Token (..))

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  | VarStmt String Expr
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
