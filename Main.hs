module Main where

import PCF.Eval (erase, eval)
import PCF.Parse (parse)
import PCF.Prelude

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified PCF.Eval as Untyped
import qualified Text.Megaparsec as Mega (errorBundlePretty)

main :: IO ()
main = do
  src <- TIO.getContents
  val <- interpret src
  TIO.putStrLn (pretty val)

interpret :: Text -> IO Untyped.Expr
interpret src =
  case parse src of
    Left e ->
      exitWithError (Text.pack (Mega.errorBundlePretty e))

    Right expr ->
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
