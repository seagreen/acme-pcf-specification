{-# LANGUAGE QuasiQuotes #-}

module PCF.Test.Eval where

import Data.String.QQ (s)
import PCF.Prelude

data TestCase = TestCase
  { name :: Text
  , description :: Text
  , result :: Result
  , source :: Text
  } deriving (Eq, Show)

data Result
  = BoolVal Bool
  | NatVal Natural
  | GenericSuccess
  deriving (Eq, Show, Ord)

tests :: [TestCase]
tests =
  [ TestCase "lam" mempty GenericSuccess [s|\x : Nat. x|]
  , TestCase "app" mempty GenericSuccess [s|(\x : Nat. x) 1|]

  , TestCase "let" mempty (NatVal 1) [s|let a = 1 in a|]

  , TestCase "if-then-else" mempty (NatVal 1) [s|if true then 1 else 0|]

  , TestCase "suc" mempty (NatVal 1) [s|suc 0|]
  , TestCase "pred" mempty (NatVal 0) [s|pred 1|]
  , TestCase "pred-0" mempty (NatVal 0) [s|pred 0|]
  , TestCase "is-zero" mempty (BoolVal True) [s|is-zero 0|]

  , TestCase "fix-simple" mempty (NatVal 1) [s|fix (\x : Nat -> Nat. 1)|]
  , TestCase "fix-realistic" mempty (NatVal 0) [s|(fix (\rec : Nat -> Nat. \x : Nat. if is-zero x then 0 else rec (pred x))) 2|]
  ]
