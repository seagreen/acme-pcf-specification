{-# LANGUAGE QuasiQuotes #-}

module PCF.Test.Eval where

import Data.Aeson hiding (Result)
import Data.String.QQ (s)
import PCF.Prelude

import qualified Data.Aeson as Aeson

data TestCase = TestCase
  { name :: Text
  , result :: Result
  , source :: Text
  } deriving (Eq, Show)

instance ToJSON TestCase where
  toJSON (TestCase{name, result, source}) =
    object
      [ "name" .= name
      , "should_succeed" .= True
      , "expected" .= expected
      , "source" .= source
      ]
    where
      expected :: Aeson.Value
      expected =
        case result of
          BoolVal b ->
            toJSON b

          NatVal n ->
            toJSON n

          GenericSuccess ->
            Null

data Result
  = BoolVal Bool
  | NatVal Natural
  | GenericSuccess
  deriving (Eq, Show, Ord)

tests :: [TestCase]
tests =
  [ TestCase "lam" GenericSuccess [s|\x : Nat. x|]
  , TestCase "app" GenericSuccess [s|(\x : Nat. x) 1|]

  , TestCase "let" (NatVal 1) [s|let a = 1 in a|]

  , TestCase "if-then-else" (NatVal 1) [s|if true then 1 else 0|]

  , TestCase "suc" (NatVal 1) [s|suc 0|]
  , TestCase "pred" (NatVal 0) [s|pred 1|]
  , TestCase "pred-0" (NatVal 0) [s|pred 0|]
  , TestCase "is-zero" (BoolVal True) [s|is-zero 0|]

  , TestCase "fix-simple" (NatVal 1) [s|fix (\x : Nat -> Nat. 1)|]
  , TestCase "fix-realistic" (NatVal 0) [s|(fix (\rec : Nat -> Nat. \x : Nat. if is-zero x then 0 else rec (pred x))) 2|]
  ]
