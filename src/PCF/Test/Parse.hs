{-# LANGUAGE QuasiQuotes #-}

module PCF.Test.Parse where

import Data.String.QQ (s)
import PCF.Prelude

data TestCase = TestCase
  { name :: Text
  , description :: Text
  , shouldParse :: Bool
  , source :: Text
  } deriving (Eq, Show)

tests :: [TestCase]
tests =
  [ TestCase "var" mempty True [s|x|]

  , TestCase "lam" mempty True [s|\x : Nat. x|]

  , TestCase "app" mempty True [s|f a|]
  , TestCase "app2" mempty True [s|f a b|]
  , TestCase "app-parens" mempty True [s|f (a b)|]
  , TestCase "app-extraneous-parens" mempty True [s|(f a) b|]

  , TestCase "let" mempty True [s|let a = 1 in a|]
  , TestCase "fix" mempty True [s|fix (\x : Nat -> Nat. 1)|]

  , TestCase "bool-true" mempty True [s|true|]
  , TestCase "bool-false" mempty True [s|false|]
  , TestCase "if-then-else" mempty True [s|if true then 1 else 0|]

  , TestCase "nat" mempty True [s|1|]

  , TestCase "type-bool" mempty True [s|\x : Bool. x|]
  , TestCase "type-bool-to-bool" mempty True [s|\x : Bool -> Bool. x|]
  , TestCase "type-bool3" mempty True [s|\x : Bool -> Bool -> Bool. x|]
  -- , TestCase "type-extraneous-parens" mempty True "\\x : Bool -> (Bool -> Bool). x"
  -- , TestCase "type-" mempty True "\\x : (Bool -> Bool) -> Bool. x"

  , TestCase "ignore-newlines" mempty True [s|
f a
b
  c
|]
  ]
