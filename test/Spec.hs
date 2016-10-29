import Test.Tasty

import ReaderTests

tests :: TestTree
tests = testGroup "Tests" [readerTests]

main :: IO ()
main = defaultMain tests
