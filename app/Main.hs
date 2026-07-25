module Main (main) where

import Control.Monad.State
import Lib
import Data.Map (empty)

main :: IO ()
main = do
  putStrLn "\n\nWelcome to LOX interpreter written in Haskell\n"
  runREPL empty
