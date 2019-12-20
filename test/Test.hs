module Main where

import Data.Aeson
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
import qualified PCF.Test.Eval as TestEval
import qualified PCF.Test.Parse as TestParse
import qualified PCF.Test.Typecheck as TestTC
import qualified Text.Megaparsec as Mega (errorBundlePretty)

data JsonTests = JsonTests
  { parseTests :: [TestParse.TestCase]
  , typecheckTests :: [TestTC.TestCase]
  , evalTests :: [TestEval.TestCase]
  } deriving (Eq, Show, Generic)

instance ToJSON JsonTests where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

tests :: JsonTests
tests =
  JsonTests TestParse.tests TestTC.tests TestEval.tests

writeJsonFile :: IO ()
writeJsonFile =
  LBS.writeFile
    "./misc/generated/test-cases.json"
    (encodePretty tests)

asMarkdownExamples :: Text
asMarkdownExamples =
  Text.concat
    [ "# Parsing examples\n\n"
    , foldMap parseToMd (parseTests tests)
    , "# Typechecking examples\n\n"
    , foldMap typecheckToMd (typecheckTests tests)
    , "# Evaluation examples\n\n"
    , foldMap evalToMd (evalTests tests)
    ]
  where
    parseToMd :: TestParse.TestCase -> Text
    parseToMd TestParse.TestCase{TestParse.name, TestParse.shouldSucceed, TestParse.source} =
      Text.unlines
        [ "### " <> name
        , ""
        , if shouldSucceed then "Should succeed:" else "Should fail:"
        , "```"
        , source
        , "```"
        , ""
        ]

    typecheckToMd :: TestTC.TestCase -> Text
    typecheckToMd TestTC.TestCase{TestTC.name, TestTC.shouldSucceed, TestTC.source} =
      Text.unlines
        [ "### " <> name
        , ""
        , if shouldSucceed then "Should succeed:" else "Should fail:"
        , "```"
        , source
        , "```"
        , ""
        ]

    evalToMd :: TestEval.TestCase -> Text
    evalToMd TestEval.TestCase{TestEval.name, TestEval.expected, TestEval.source} =
      Text.unlines
        [ "### " <> name
        , ""
        , "Expected: " <> case expected of
                            BoolExpected True ->
                              "true"

                            BoolExpected False ->
                              "false"

                            NatExpected n ->
                              Text.pack (show n)

                            GenericSuccess ->
                              "<success>"
        , "```"
        , source
        , "```"
        , ""
        ]

writeMarkdownFile :: IO ()
writeMarkdownFile =
  TIO.writeFile
    "./misc/generated/examples.md"
    asMarkdownExamples

main :: IO ()
main = do
  writeJsonFile
  writeMarkdownFile
  hspec do
    describe "parser" (for_ (parseTests tests) parseTest)
    describe "typecheck" (for_ (typecheckTests tests) typecheckTest)
    describe "eval" (for_ (evalTests tests) evalTest)

parseTest :: TestParse.TestCase -> Spec
parseTest TestParse.TestCase{TestParse.name, TestParse.shouldSucceed, TestParse.source} =
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

typecheckTest :: TestTC.TestCase -> Spec
typecheckTest TestTC.TestCase{TestTC.name, TestTC.shouldSucceed, TestTC.source} =
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

evalTest :: TestEval.TestCase -> Spec
evalTest TestEval.TestCase{TestEval.name, TestEval.expected, TestEval.source} =
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
