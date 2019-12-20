module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import PCF.Eval (Value(..), eval)
import PCF.Parse (parse)
import PCF.Prelude hiding (parseTest)
import PCF.Test.Eval (Expected(..))
import PCF.Typecheck (typecheck)
import Test.Hspec

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified PCF.Test.Eval as TE
import qualified PCF.Test.Parse as TP
import qualified PCF.Test.Suite as Suite
import qualified PCF.Test.Typecheck as TT
import qualified Text.Megaparsec as Mega (errorBundlePretty)

writeJsonFile :: IO ()
writeJsonFile =
  LBS.writeFile
    "./misc/generated/test-cases.json"
    (encodePretty Suite.tests)

writeMarkdownFile :: IO ()
writeMarkdownFile =
  TIO.writeFile
    "./misc/generated/examples.md"
    Suite.asMarkdownExamples

main :: IO ()
main = do
  writeJsonFile
  writeMarkdownFile
  hspec do
    describe "parser"    (for_ (Suite.parseTests     Suite.tests) parseTest)
    describe "typecheck" (for_ (Suite.typecheckTests Suite.tests) typecheckTest)
    describe "eval"      (for_ (Suite.evalTests      Suite.tests) evalTest)

parseTest :: TP.TestCase -> Spec
parseTest TP.TestCase{TP.name, TP.shouldSucceed, TP.source} =
  it
    (Text.unpack name)
    (let
       res = Bifunctor.first Mega.errorBundlePretty (parse source)
     in
       if shouldSucceed
         then
           res `shouldSatisfy` isRight

         else
           res `shouldSatisfy` isLeft)

typecheckTest :: TT.TestCase -> Spec
typecheckTest TT.TestCase{TT.name, TT.shouldSucceed, TT.source} =
  it
    (Text.unpack name)
    case parse source of
      Left e ->
        fail (Mega.errorBundlePretty e)

      Right expr -> do
        let res = typecheck mempty expr
        if shouldSucceed
          then
            res `shouldSatisfy` isRight

          else
            res `shouldSatisfy` isLeft

evalTest :: TE.TestCase -> Spec
evalTest TE.TestCase{TE.name, TE.expected, TE.source} =
  it
    (Text.unpack name)
    case parse source of
      Left e ->
        fail (Mega.errorBundlePretty e)

      Right expr -> do
        case typecheck mempty expr of
          Left e ->
            fail (show e)

          Right _ ->
            pure ()

        let res = eval mempty expr
        case expected of
          BoolExpected b ->
            res `shouldBe` Right (BoolVal b)

          NatExpected n ->
            res `shouldBe` Right (NatVal n)

          GenericSuccess ->
            res `shouldSatisfy` isRight
