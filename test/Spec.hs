import Test.Tasty

import ReaderTests
import EvalTests

tests :: TestTree
tests = testGroup "Tests" [readerTests, evalTests]

main :: IO ()
main = defaultMain tests
