{-# LANGUAGE QuasiQuotes #-}

module PcfTest.Eval where

import Data.Aeson
import Data.String.QQ (s)
import PCF.Prelude

import qualified Data.Aeson as Aeson

data TestCase = TestCase
  { name :: Text
  , expected :: Expected
  , source :: Text
  } deriving (Eq, Show)

instance ToJSON TestCase where
  toJSON TestCase{name, expected, source} =
    object
      [ "name" .= name
      , "expected" .= expectedJson
      , "source" .= source
      ]
    where
      expectedJson :: Aeson.Value
      expectedJson =
        case expected of
          BoolExpected b ->
            toJSON b

          NatExpected n ->
            toJSON n

          GenericSuccess ->
            Null

data Expected
  = BoolExpected Bool
  | NatExpected Natural
  | GenericSuccess
  deriving (Eq, Show, Ord)

tests :: [TestCase]
tests =
  [ TestCase "lam" GenericSuccess [s|\x : Nat. x|]
  , TestCase "app" GenericSuccess [s|(\x : Nat. x) 1|]

  , TestCase "let" (NatExpected 1) [s|let a = 1 in a|]

  , TestCase "if-then-else" (NatExpected 1) [s|if true then 1 else 0|]

  , TestCase "suc" (NatExpected 1) [s|suc 0|]
  , TestCase "pred" (NatExpected 0) [s|pred 1|]
  , TestCase "pred-0" (NatExpected 0) [s|pred 0|]
  , TestCase "is-zero" (BoolExpected True) [s|is-zero 0|]

  , TestCase "fix-simple" (NatExpected 1) [s|fix (\x : Nat. 1)|]
  , TestCase "fix-realistic" (NatExpected 0) [s|fix (\rec : Nat -> Nat. \x : Nat. if is-zero x then 0 else rec (pred x)) 2|]

  -- Keep this synced with the README:
  , TestCase "detailed-example" (NatExpected 7) [s|
let
  add =
    fix
      (\recurse : Nat -> Nat -> Nat.
        \x : Nat. \y : Nat.
          if is-zero x
            then
              y

            else
              recurse (pred x) (suc y))
in
  add 3 4
|]
  ]
