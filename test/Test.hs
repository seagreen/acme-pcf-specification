module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import PCF.Prelude hiding (parseTest)
import PCF.Eval (eval, erase)
import PCF.Parse (parse)
import PCF.Test.Eval (Result(..))
import Test.Hspec

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified PCF.Eval as Untyped
import qualified PCF.Test.Eval
import qualified PCF.Test.Parse
import qualified Text.Megaparsec as Mega

data JsonTests = JsonTests
  { parseTests :: [PCF.Test.Parse.TestCase]
  , evalTests :: [PCF.Test.Eval.TestCase]
  } deriving (Eq, Show, Generic)

instance ToJSON JsonTests where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

writeJsonFile :: IO ()
writeJsonFile =
  LBS.writeFile
    "./misc/generated/test-cases.json"
    (encodePretty (JsonTests PCF.Test.Parse.tests PCF.Test.Eval.tests))

main :: IO ()
main = do
  writeJsonFile
  hspec do
    describe "parser" (for_ PCF.Test.Parse.tests parseTest)
    describe "eval" (for_ PCF.Test.Eval.tests evalTest)

parseTest :: PCF.Test.Parse.TestCase -> Spec
parseTest (PCF.Test.Parse.TestCase name _ src) =
  it
    (Text.unpack name)
    case parse src of
      Left e ->
        fail (Mega.errorBundlePretty e)

      Right _ ->
        True `shouldBe` True

evalTest :: PCF.Test.Eval.TestCase -> Spec
evalTest (PCF.Test.Eval.TestCase name expected src) =
  it
    (Text.unpack name)
    case parse src of
      Left e ->
        fail (Mega.errorBundlePretty e)

      Right expr ->
        let res = eval mempty (erase expr)
        in case expected of
          BoolVal b ->
            res `shouldBe` Right (Untyped.BoolLit b)

          NatVal n ->
            res `shouldBe` Right (Untyped.NatLit n)

          GenericSuccess ->
            res `shouldSatisfy` isRight
