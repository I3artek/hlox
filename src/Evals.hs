module Evals where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State
import Data.Data (typeOf)
import Exprs (BinaryExpr (..), Expr (..), GroupingExpr (..), LiteralExpr (..), UnaryExpr (..))
import Stmts (Stmt (..))
import Tokens (Token (..))

data Value
  = LoxString String
  | LoxNumber Double
  | LoxObject String -- for now
  | LoxBool Bool
  | LoxNil
  deriving (Eq, Ord)

instance Show Value where
  show (LoxString s) = show s
  show (LoxNumber n) = show n
  show (LoxObject s) = show s
  show (LoxBool b) = show b
  show LoxNil = ""

type RuntimeError = String

-- placeholder type
type ScopeState = Int

type Scope a = ExceptT RuntimeError (StateT ScopeState IO) a

runtimeError :: String -> Scope ()
runtimeError msg = do
  throwError $ "Runtime Error: " ++ msg

evalError :: String -> Scope Value
evalError msg = do
  throwError $ "Runtime Error: " ++ msg

execScope :: [Stmt] -> Scope ()
execScope [] = return ()
execScope (s : rest) = do
  execStmt s
  execScope rest

execProgram :: [Stmt] -> IO ()
execProgram stmts = do
  errors <- evalStateT (runExceptT $ execScope stmts) (0)
  case errors of
    Left err -> print err
    Right _ -> return ()

execStmt :: Stmt -> Scope ()
execStmt (ExprStmt e) = do
  _ <- evalExpr e
  return ()
execStmt (PrintStmt e) = do
  val <- evalExpr e
  lift $ lift $ print val
  return ()

evalExpr :: Expr -> Scope Value
evalExpr (Binary e) = do
  left <- evalExpr $ binaryLeft e
  right <- evalExpr $ binaryRight e
  let op = binaryOperator e
  evalBinaryExpr left op right
evalExpr (Grouping e) = evalExpr $ groupedExpression e
evalExpr (Literal e) = evalLiteral $ value e
evalExpr (Unary e) = do
  right <- evalExpr $ unaryRight e
  let op = unaryOperator e
  evalUnaryExpr op right

evalBinaryExpr :: Value -> Token -> Value -> Scope Value
evalBinaryExpr (LoxString a) PLUS (LoxString b) = return $ LoxString (a ++ b)
evalBinaryExpr (LoxNumber a) PLUS (LoxNumber b) = return $ LoxNumber (a + b)
evalBinaryExpr (LoxString a) PLUS v = return $ LoxString (a ++ show v)
evalBinaryExpr (v) PLUS (LoxString b) = return $ LoxString (show v ++ b)
evalBinaryExpr (LoxNumber a) MINUS (LoxNumber b) = return $ LoxNumber (a - b)
evalBinaryExpr (LoxNumber _) SLASH (LoxNumber 0) = evalError "Division by zero not allowed!"
evalBinaryExpr (LoxNumber a) SLASH (LoxNumber b) = return $ LoxNumber (a / b)
evalBinaryExpr (LoxNumber a) STAR (LoxNumber b) = return $ LoxNumber (a * b)
evalBinaryExpr v1 BANG_EQUAL v2 = return $ LoxBool $ v1 /= v2
evalBinaryExpr v1 EQUAL_EQUAL v2 = return $ LoxBool $ v1 == v2
evalBinaryExpr v1 GREATER v2 = return $ LoxBool $ v1 > v2
evalBinaryExpr v1 GREATER_EQUAL v2 = return $ LoxBool $ v1 >= v2
evalBinaryExpr v1 LESS v2 = return $ LoxBool $ v1 < v2
evalBinaryExpr v1 LESS_EQUAL v2 = return $ LoxBool $ v1 <= v2
evalBinaryExpr t1 op t2 = evalError $ "Not possible to perform " ++ show t1 ++ " " ++ show op ++ " " ++ show t2

evalLiteral :: Token -> Scope Value
evalLiteral (STRING s) = return $ LoxString s
evalLiteral (NUMBER n) = return $ LoxNumber n
evalLiteral (IDENTIFIER x) = return $ LoxObject x
evalLiteral FALSE = return $ LoxBool False
evalLiteral TRUE = return $ LoxBool True
evalLiteral _ = return LoxNil

evalUnaryExpr :: Token -> Value -> Scope Value
evalUnaryExpr MINUS (LoxNumber b) = return $ LoxNumber (-b)
evalUnaryExpr BANG t = return $ LoxBool $ not $ isTruthy t
evalUnaryExpr op t = evalError $ "Not possible to perform " ++ show op ++ " " ++ show t

isTruthy :: Value -> Bool
isTruthy (LoxBool False) = False
isTruthy LoxNil = False
isTruthy _ = True
