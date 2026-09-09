import Data.Map (empty)
import Lib

main :: IO ()
main = do
  selfTest

selfTest :: IO ()
selfTest = runSourceFile [empty] "test/tests.lox"
