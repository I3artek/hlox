module Evals where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State
import Data.Map (Map, empty, insert, member, notMember, (!))
import Exprs
  ( AssignmentExpr (..),
    BinaryExpr (..),
    CallExpr (..),
    Expr (..),
    GroupingExpr (..),
    LiteralExpr (..),
    LogicalExpr (..),
    UnaryExpr (..),
    VariableExpr (..),
  )
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

type Env = Map String Value

type ScopeState = [Env]

type Scope a = ExceptT RuntimeError (StateT ScopeState IO) a

scopeIO :: IO a -> Scope a
scopeIO io = do
  lift $ lift io

runtimeError :: String -> Scope a
runtimeError msg = do
  throwError $ "Runtime Error: " ++ msg

evalError :: String -> Scope Value
evalError msg = do
  throwError $ "Runtime Error: " ++ msg

defineInScope :: String -> Value -> Scope ()
defineInScope name val = do
  envs <- get
  case envs of
    (s : copes) -> do put $ (insert name val s) : copes
    -- The following is a bug in the interpreter itself, so we can crash
    [] -> error "Left global scope!"

assignInScope :: String -> Value -> Scope ()
assignInScope name val = do
  envs <- get
  let (without, with) = break (member name) envs
  case with of
    (found : rest) -> put $ without ++ (insert name val found) : rest
    [] -> runtimeError $ "Variable '" ++ name ++ "' is undefined"

lookupInScope :: String -> Scope Value
lookupInScope name = do
  envs <- get
  let maybeFound = dropWhile (notMember name) envs
  case maybeFound of
    (found : _) -> return $ found ! name
    [] -> runtimeError $ "Variable '" ++ name ++ "' is undefined"

execScope :: [Stmt] -> Scope ()
execScope [] = return ()
execScope (s : rest) = do
  execStmt s
  execScope rest

execProgram :: [Stmt] -> IO ()
execProgram stmts = do
  errors <- evalStateT (runExceptT $ execScope stmts) ([empty])
  case errors of
    Left err -> print err
    Right _ -> return ()

execStmt :: Stmt -> Scope ()
execStmt (ExprStmt e) = do
  _ <- evalExpr e
  return ()
execStmt (IfStmt cond thenB elseB) = do
  val <- evalExpr cond
  if isTruthy val
    then execStmt thenB
    else execStmt elseB
execStmt (PrintStmt e) = do
  val <- evalExpr e
  lift $ lift $ print val
  return ()
execStmt loop@(WhileStmt cond body) = do
  val <- evalExpr cond
  if isTruthy val
    then do
      execStmt body
      execStmt loop
    else return ()
execStmt (VarStmt name e) = do
  val <- evalExpr e
  defineInScope name val
execStmt (BlockStmt stmts) = do
  newScope
  execScope stmts
  exitScope
execStmt NOPStmt = return ()

newScope :: Scope ()
newScope = modify (\envs -> empty : envs)

-- Make sure we always have at least one (global) env
-- Throw an error otherwise as this is a bug and not a user error
exitScope :: Scope ()
exitScope = do
  envs <- get
  case envs of
    (_ : e : nvs) -> put (e : nvs)
    _ -> error "Left global scope!"

toCallable :: Value -> Scope Value
toCallable callee = return callee

evalExprList :: [Expr] -> Scope [Value]
evalExprList [] = return []
evalExprList (first : rest) = do
  fEval <- evalExpr first
  rEval <- evalExprList rest
  return $ fEval : rEval

evalExpr :: Expr -> Scope Value
evalExpr (Assignment e) = do
  val <- evalExpr $ assignmentValue e
  assignInScope (assignmentName e) val
  return val
evalExpr (Binary e) = do
  left <- evalExpr $ binaryLeft e
  right <- evalExpr $ binaryRight e
  let op = binaryOperator e
  evalBinaryExpr left op right
evalExpr (Call e) = do
  callee <- evalExpr $ callCallee e
  callable <- toCallable callee
  args <- evalExprList (callArguments e)
  return LoxNil
evalExpr (Grouping e) = evalExpr $ groupedExpression e
evalExpr (Literal e) = evalLiteral $ value e
evalExpr (Logical e) = do
  left <- evalExpr $ logicalLeft e
  case logicalOperator e of
    OR -> do
      if isTruthy left
        then return left
        else evalExpr $ logicalRight e
    AND -> do
      if not $ isTruthy left
        then return left
        else evalExpr $ logicalRight e
    _ -> error ""
evalExpr (Unary e) = do
  right <- evalExpr $ unaryRight e
  let op = unaryOperator e
  evalUnaryExpr op right
evalExpr (Variable e) = lookupInScope $ varName e

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
