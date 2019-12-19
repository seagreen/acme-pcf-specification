{-# LANGUAGE QuasiQuotes #-}

module PCF.Test.Typecheck where

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
  [ TestCase "bool-literal" True [s|true|]
  , TestCase "is-zero" True [s|is-zero 1|]
  , TestCase "app-not-function" False [s|0 1|]
  ]
