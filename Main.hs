module Main where

import Options.Applicative
import PCF.Eval (Value(..), prettyValue, eval)
import PCF.Parse (parse)
import PCF.Prelude
import PCF.Typecheck (typecheck)

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as Mega (errorBundlePretty)

main :: IO ()
main = do
  checkForHelpFlag
  src <- TIO.getContents
  val <- interpret src
  TIO.putStrLn (prettyValue val)

interpret :: Text -> IO Value
interpret src =
  case parse src of
    Left e ->
      exitWithError (Text.pack (Mega.errorBundlePretty e))

    Right expr -> do
      case typecheck mempty expr of
        Left e ->
          exitWithError (Text.pack (show e))

        Right _ ->
          pure ()

      case eval mempty expr of
        Left e ->
          exitWithError (Text.pack (show e))

        Right val ->
          pure val

checkForHelpFlag :: IO ()
checkForHelpFlag =
  customExecParser (prefs showHelpOnError) configParser

configParser :: ParserInfo ()
configParser =
  info
    (helper <*> pure ())
    (progDesc "Interpret PCF source from STDIN")
