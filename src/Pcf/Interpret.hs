module Pcf.Interpret where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text.IO as TIO
import Pcf.Eval (Value (..), eval, prettyValue)
import qualified Pcf.Eval as Eval
import Pcf.Parse (ParserErrorBundle, parse)
import Pcf.Prelude
import Pcf.Typecheck (typecheck)
import qualified Pcf.Typecheck as Typecheck
import System.Exit as X (ExitCode (..))
import System.IO as X (hPutStrLn)
import qualified Text.Megaparsec as Mega (errorBundlePretty)

data Error
  = ErrorParse ParserErrorBundle
  | ErrorTypecheck Typecheck.Error
  | ErrorEval Eval.Error
  deriving (Eq, Show)

interpret :: Text -> Either Error Value
interpret src = do
  expr <- Bifunctor.first ErrorParse (parse src)
  _type <- Bifunctor.first ErrorTypecheck (typecheck mempty expr)
  Bifunctor.first ErrorEval (eval mempty expr)

interpretIO :: Text -> IO ()
interpretIO src =
  case interpret src of
    Left e -> do
      let (exitCode, msg) = errorAction e
      hPutStrLn stderr msg
      exitWith (ExitFailure (fromIntegral exitCode))
    Right val ->
      TIO.putStrLn (prettyValue val)

errorAction :: Error -> (Natural, String)
errorAction = \case
  ErrorParse e ->
    (parseError, Mega.errorBundlePretty e)
  ErrorTypecheck e ->
    (typecheckError, show e)
  ErrorEval e ->
    (otherError, show e)
  where
    parseError :: Natural
    parseError =
      2

    typecheckError :: Natural
    typecheckError =
      3

    otherError :: Natural
    otherError =
      1
