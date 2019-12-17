module Main where

import PCF.Prelude hiding (parseTest)
import Test.Hspec

import PCF.Eval (eval, erase)
import PCF.Parse (parse)
import PCF.Test.Eval (Result(..))

import qualified Data.Text as Text
import qualified PCF.Eval as Untyped
import qualified PCF.Test.Eval
import qualified PCF.Test.Parse
import qualified Text.Megaparsec as Mega

main :: IO ()
main =
  hspec do
    describe "parser" (for_ PCF.Test.Parse.tests parseTest)
    describe "eval" (for_ PCF.Test.Eval.tests evalTest)

parseTest :: PCF.Test.Parse.TestCase -> Spec
parseTest (PCF.Test.Parse.TestCase name _desc _ src) =
  it
    (Text.unpack name)
    case parse src of
      Left e ->
        fail (Mega.errorBundlePretty e)

      Right _ ->
        True `shouldBe` True

evalTest :: PCF.Test.Eval.TestCase -> Spec
evalTest (PCF.Test.Eval.TestCase name _desc expected src) =
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
