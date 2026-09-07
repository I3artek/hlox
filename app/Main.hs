module Main (main) where

import Data.Map (empty)
import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "\n\nWelcome to LOX interpreter written in Haskell\n"
      runREPL [empty]
    [filename] -> do
      runSourceFile [empty] filename
    _ -> print "Expected 0 or 1 argument (filename)"
