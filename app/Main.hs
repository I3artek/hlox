module Main (main) where

import Control.Monad.State
import Lib

main :: IO ()
main = do
  putStrLn "\n\nWelcome to LOX interpreter written in Haskell\n"
  evalStateT repl initGlobalState
