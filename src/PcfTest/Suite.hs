module PcfTest.Suite where

import Data.Aeson
import PCF.Prelude hiding (parseTest)
import PcfTest.Eval (Expected(..))

import qualified Data.Text as Text
import qualified PcfTest.Eval as TE
import qualified PcfTest.Parse as TP
import qualified PcfTest.Typecheck as TT

data JsonTests = JsonTests
  { parseTests :: [TP.TestCase]
  , typecheckTests :: [TT.TestCase]
  , evalTests :: [TE.TestCase]
  } deriving (Eq, Show, Generic)

instance ToJSON JsonTests where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

tests :: JsonTests
tests =
  JsonTests TP.tests TT.tests TE.tests

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
    parseToMd :: TP.TestCase -> Text
    parseToMd TP.TestCase{TP.name, TP.shouldSucceed, TP.source} =
      Text.unlines
        [ "### " <> name
        , ""
        , if shouldSucceed then "Should succeed:" else "Should fail:"
        , "```"
        , source
        , "```"
        , ""
        ]

    typecheckToMd :: TT.TestCase -> Text
    typecheckToMd TT.TestCase{TT.name, TT.shouldSucceed, TT.source} =
      Text.unlines
        [ "### " <> name
        , ""
        , if shouldSucceed then "Should succeed:" else "Should fail:"
        , "```"
        , source
        , "```"
        , ""
        ]

    evalToMd :: TE.TestCase -> Text
    evalToMd TE.TestCase{TE.name, TE.expected, TE.source} =
      Text.unlines
        [ "### " <> name
        , ""
        , "Expected: " <> case expected of
                            BoolExpected True ->
                              "true"

                            BoolExpected False ->
                              "false"

                            NatExpected n ->
                              show n

                            GenericSuccess ->
                              "<success>"
        , "```"
        , source
        , "```"
        , ""
        ]
