module Main (main) where

import Control.Monad.State
import Lib

main :: IO ()
main = evalStateT repl initGlobalState
