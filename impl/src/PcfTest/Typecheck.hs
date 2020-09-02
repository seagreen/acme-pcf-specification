{-# LANGUAGE QuasiQuotes #-}

module PcfTest.Typecheck where

import Data.Aeson
import Data.String.QQ (s)
import Pcf.Prelude

data TestCase = TestCase
  { name :: Text,
    shouldSucceed :: Bool,
    source :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON TestCase where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

tests :: [TestCase]
tests =
  [ TestCase "bool-literal" True [s|true|],
    TestCase "nat-literal" True [s|1|],
    TestCase "is-zero" True [s|is-zero 1|],
    TestCase "is-zero-applied-to-bool" False [s|is-zero true|],
    TestCase "lambda" True [s|(\x : Nat. x)|],
    TestCase "not-in-scope" False [s|a|],
    TestCase "app-not-function" False [s|0 1|],
    TestCase "applied-wrong-type" False [s|(\x : Nat. x) true|],
    TestCase "if-not-bool" False [s|if 1 then true else false|],
    TestCase "if-statement-mismatch" False [s|if true then 1 else false|],
    TestCase "fix" True [s|fix (\x : Nat. 1)|],
    TestCase "fix-not-lambda" False [s|fix 1|],
    TestCase "fix-type-mismatch" False [s|fix (\x : Nat -> Nat. 1)|]
  ]
