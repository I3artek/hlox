module Evals where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State
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

type MaybeValue = Either String Value

-- placeholder type
type ScopeState = Int

type Scope a = StateT ScopeState IO a

runtimeError :: String -> Scope ()
runtimeError msg = do
  lift $ putStrLn $ "Runtime Error: " ++ msg

execScope :: [Stmt] -> Scope ()
execScope [] = return ()
execScope (s : rest) = do
  execStmt s
  execScope rest

execProgram :: [Stmt] -> IO ()
execProgram stmts = evalStateT (execScope stmts) (0)

execStmt :: Stmt -> Scope ()
execStmt (ExprStmt e) = do
  let val = evalExpr e
  case val of
    Left v -> runtimeError v
    Right _ -> return ()
execStmt (PrintStmt e) = do
  let val = evalExpr e
  case val of
    Left v -> runtimeError v
    Right v -> lift $ print v

evalExpr :: Expr -> MaybeValue
evalExpr (Binary e) = do
  left <- evalExpr $ binaryLeft e
  right <- evalExpr $ binaryRight e
  let op = binaryOperator e
  evalBinaryExpr left op right
evalExpr (Grouping e) = evalExpr $ groupedExpression e
evalExpr (Literal e) = Right $ evalLiteral $ value e
evalExpr (Unary e) = do
  right <- evalExpr $ unaryRight e
  let op = unaryOperator e
  Right $ evalUnaryExpr op right

evalBinaryExpr :: Value -> Token -> Value -> MaybeValue
evalBinaryExpr (LoxString a) PLUS (LoxString b) = Right $ LoxString (a ++ b)
evalBinaryExpr (LoxNumber a) PLUS (LoxNumber b) = Right $ LoxNumber (a + b)
evalBinaryExpr (LoxString a) PLUS v = Right $ LoxString (a ++ show v)
evalBinaryExpr (v) PLUS (LoxString b) = Right $ LoxString (show v ++ b)
evalBinaryExpr (LoxNumber a) MINUS (LoxNumber b) = Right $ LoxNumber (a - b)
evalBinaryExpr (LoxNumber _) SLASH (LoxNumber 0) = throwError "Division by zero not allowed!"
evalBinaryExpr (LoxNumber a) SLASH (LoxNumber b) = Right $ LoxNumber (a / b)
evalBinaryExpr (LoxNumber a) STAR (LoxNumber b) = Right $ LoxNumber (a * b)
evalBinaryExpr v1 BANG_EQUAL v2 = Right $ LoxBool $ v1 /= v2
evalBinaryExpr v1 EQUAL_EQUAL v2 = Right $ LoxBool $ v1 == v2
evalBinaryExpr v1 GREATER v2 = Right $ LoxBool $ v1 > v2
evalBinaryExpr v1 GREATER_EQUAL v2 = Right $ LoxBool $ v1 >= v2
evalBinaryExpr v1 LESS v2 = Right $ LoxBool $ v1 < v2
evalBinaryExpr v1 LESS_EQUAL v2 = Right $ LoxBool $ v1 <= v2
evalBinaryExpr t1 op t2 = throwError $ "Not possible to perform " ++ show t1 ++ " " ++ show op ++ " " ++ show t2

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
