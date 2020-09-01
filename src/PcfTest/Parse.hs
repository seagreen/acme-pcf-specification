{-# LANGUAGE QuasiQuotes #-}

module PcfTest.Parse where

import Data.Aeson
import Data.String.QQ (s)
import PCF.Prelude

data TestCase = TestCase
  { name :: Text
  , shouldSucceed :: Bool
  , source :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON TestCase where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

tests :: [TestCase]
tests =
  [ TestCase "var" True [s|x|]

  , TestCase "lam" True [s|\x : Nat. x|]

  , TestCase "app" True [s|f a|]
  , TestCase "app2" True [s|f a b|]
  , TestCase "app-parens" True [s|f (a b)|]
  , TestCase "app-extraneous-parens" True [s|(f a) b|]

  , TestCase "let" True [s|let a = 1 in a|]
  , TestCase "fix" True [s|fix (\x : Nat -> Nat. 1)|]

  , TestCase "bool-true" True [s|true|]
  , TestCase "bool-false" True [s|false|]
  , TestCase "if-then-else" True [s|if true then 1 else 0|]

  , TestCase "nat" True [s|1|]

  , TestCase "type-bool" True [s|\x : Bool. x|]
  , TestCase "type-bool-to-bool" True [s|\x : Bool -> Bool. x|]
  , TestCase "type-bool3" True [s|\x : Bool -> Bool -> Bool. x|]
  , TestCase "type-extraneous-parens" True "\\x : Bool -> (Bool -> Bool). x"
  , TestCase "type-function-as-argument" True "\\x : (Bool -> Bool) -> Bool. x"

  , TestCase "ignore-newlines" True [s|
f a
b
  c
|]

  , TestCase "invalid-symbol" False [s|!|]
  ]
