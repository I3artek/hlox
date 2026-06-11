module Lib (initGlobalState, repl) where

import Control.Monad.State
import Evals (evalExpr)
import Exprs
import System.IO
import Tokens (scanTokens)

-- This should hold information like the map of all identidiers.
-- In repl, it should be preserved between lines ofc.
data GlobalState = GlobalState {placeholder :: Int}

initGlobalState :: GlobalState
initGlobalState = GlobalState {placeholder = 0}

type REPL a = StateT GlobalState IO a

repl :: REPL ()
repl = do
  lift $ putStrLn "> HLox REPL:"
  finish <- lift isEOF
  if finish
    then do return ()
    else do
      line <- lift getLine
      run line
      repl

run :: String -> REPL ()
run s = do
  let (tkns, errors) = scanTokens s
  case errors of
    [] -> do
      -- lift $ print tokens
      let tree = parse tkns
      lift $ print tree
      lift $ print $ evalExpr tree
    errs -> lift $ print errs
