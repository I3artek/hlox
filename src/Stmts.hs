module Stmts where

import Control.Monad.State (evalState)
import Exprs
import Tokens (Token (..))

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  deriving (Show)

statement :: Parser Stmt
statement = do
  maybePrint <- match [PRINT]
  case maybePrint of
    Just _ -> printStatement
    Nothing -> exprStatement

printStatement :: Parser Stmt
printStatement = do
  dupa <- expression
  semicolon <- match [SEMICOLON]
  case semicolon of
    Just _ -> do
      return $ PrintStmt dupa
    Nothing -> error "Expect ';' after value."

exprStatement :: Parser Stmt
exprStatement = do
  expr <- expression
  semicolon <- match [SEMICOLON]
  case semicolon of
    Just _ -> return $ ExprStmt expr
    Nothing -> error "Expect ';' after value."

statements :: Parser [Stmt]
statements = do
  isEOF <- peek
  case isEOF of
    EOF -> return []
    _ -> do
      current <- statement
      rest <- statements
      return (current : rest)

parse :: [Token] -> [Stmt]
parse ts = evalState statements (ParserState ts)
