module Main where

import Options.Applicative
import PCF.Eval (erase, eval)
import PCF.Parse (parse)
import PCF.Prelude
import PCF.Typecheck (typecheck)

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified PCF.Eval as Untyped
import qualified Text.Megaparsec as Mega (errorBundlePretty)

main :: IO ()
main = do
  checkForHelpFlag
  src <- TIO.getContents
  val <- interpret src
  TIO.putStrLn (pretty val)

interpret :: Text -> IO Untyped.Expr
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

      case eval mempty (erase expr) of
        Left e ->
          exitWithError (Text.pack (show e))

        Right val ->
          pure val

pretty :: Untyped.Expr -> Text
pretty = \case
  Untyped.BoolLit b ->
    if b then "true" else "false"

  Untyped.NatLit n ->
    Text.pack (show n)

  other ->
    Text.pack (show other)

checkForHelpFlag :: IO ()
checkForHelpFlag =
  customExecParser (prefs showHelpOnError) configParser

configParser :: ParserInfo ()
configParser =
  info
    (helper <*> pure ())
    (progDesc "Interpret PCF source from STDIN")
