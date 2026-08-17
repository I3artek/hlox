module Lib (runREPL) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State
import Evals (RuntimeException (..), Scope, ScopeState, execScope, scopeIO)
import Stmts (parse)
import System.IO
import Tokens (scanTokens)

repl :: Scope ()
repl = do
  scopeIO $ putStrLn "> HLox REPL:"
  finish <- scopeIO isEOF
  if finish
    then do return ()
    else do
      line <- scopeIO getLine
      run line
      repl

runREPL :: ScopeState -> IO ()
runREPL initialScope = do
  (errors, scope) <- runStateT (runExceptT $ repl) (initialScope)
  case errors of
    Left (RuntimeError err) -> do print err; runREPL scope
    Left (ReturnException val) -> do print val; runREPL scope
    Right _ -> return ()

run :: String -> Scope ()
run s = do
  let (tkns, errors) = scanTokens s
  case errors of
    [] -> do
      stmts <- scopeIO $ parse tkns
      execScope stmts
      return ()
    errs -> scopeIO $ print errs
