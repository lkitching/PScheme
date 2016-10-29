module ReaderTests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck

import PScheme.Reader

genIdent :: Gen String
genIdent = listOf1 $ choose ('a', 'z')

genString :: Gen String
genString = fmap unwords $ listOf genWord where
  genWord = listOf1 $ elements ['a' .. 'z']

genValue :: Gen Value
genValue = sized genValue' where
  genPrim = oneof [
    fmap Number arbitrary,
    fmap Symbol genIdent,
    fmap Str genString,
    pure Nil
    ]
  genList n = do
    f <- genIdent
    args <- vectorOf 4 (genValue' (n `div` 2))
    pure $ Cons (Symbol f) (listToCons args)
  genValue' 0 = genPrim
  genValue' n | n > 0 = oneof [genList n, genPrim]

instance Arbitrary Value where
  arbitrary = genValue
  shrink (Cons h t) = [h, t]
  shrink _ = []

read_prop :: Value -> Bool
read_prop v = case readStringOne (show v) of
  Left _ -> False
  Right r -> v == r

readerTests :: TestTree
readerTests = testGroup "Reader Tests" [qcTests, unitTests]

qcTests :: TestTree
qcTests = testGroup "Reader quickcheck tests"
  [QC.testProperty "read" read_prop]

unitTests :: TestTree
unitTests = testGroup "Reader unit tests"
  [testCase "Unbalanced parens" $
   readStringOne ")" @?= Left UnbalancedParens,

   testCase "Invalid escape" $
   readStringOne "\"abc\\xde\"" @?= Left (InvalidEscape 'x'),

   testCase "Invalid number" $
   case (readStringOne "12ab") of
     Left (BadNumber _) -> pure ()
     Right _ -> assertFailure "Expected bad number"
  ]
