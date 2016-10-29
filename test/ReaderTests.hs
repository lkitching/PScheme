module ReaderTests where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck

import Data.List (foldl')

import PScheme.Reader

genIdent :: Gen String
genIdent = listOf1 $ choose ('a', 'z')

genString :: Gen String
genString = fmap unwords $ listOf genWord where
  genWord = listOf1 $ elements ['a' .. 'z']

instance Arbitrary NumSign where
  arbitrary = elements [PScheme.Reader.Positive, Negative]
  
genToken :: Gen Token
genToken = oneof [
  genNum,
  fmap TStr genString,
  fmap TSym genIdent,
  pure OpenParen,
  pure CloseParen,
  pure Quote,
  pure Unquote
  ] where
  genNum = do
    s <- arbitrary
    n <- arbitrary :: (Gen (NonNegative Integer))
    pure $ TNumber s (show $ getNonNegative n)

instance Arbitrary Token where
  arbitrary = genToken

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

--checks a single expression can be re-read
read_prop :: Value -> Bool
read_prop v = case readStringOne (show v) of
  Left _ -> False
  Right r -> v == r

--checks a single token can be re-read
tokenise_prop :: Token -> Bool
tokenise_prop t = case readTokens (show t) of
  Right [t'] -> t == t'
  _ -> False

needsDelim :: Token -> Bool
needsDelim (TNumber _ _) = True
needsDelim (TSym _) = True
needsDelim _ = False
  
formatTokenStream :: [Token] -> String
formatTokenStream = concat . map (\t -> if needsDelim t then ' ':(show t) else (show t))

--checks a list of tokens can be re-tokenised correctly
token_stream_prop :: [Token] -> Bool
token_stream_prop ts =
  either (const False) (== ts) $ readTokens (formatTokenStream ts)

readerTests :: TestTree
readerTests = testGroup "Reader Tests" [qcTests, unitTests]

qcTests :: TestTree
qcTests = testGroup "Reader quickcheck tests"
  [QC.testProperty "read" read_prop,
   QC.testProperty "tokenise" tokenise_prop,
   QC.testProperty "token stream" token_stream_prop]

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
