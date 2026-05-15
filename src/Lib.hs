module Lib (initGlobalState, repl) where

import Control.Monad.State
import System.IO

data GlobalState = GlobalState {placeholder :: Int}

initGlobalState :: GlobalState
initGlobalState = GlobalState {placeholder = 0}

type REPL a = StateT GlobalState IO a

repl :: REPL ()
repl = do
  finish <- lift isEOF
  if finish
    then do return ()
    else do
      line <- lift getLine
      lift $ print line
      repl
