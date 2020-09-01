module Pcf.Interpret where

import qualified Data.Text.IO as TIO
import Pcf.Eval (Value (..), eval, prettyValue)
import Pcf.Parse (parse)
import Pcf.Prelude
import Pcf.Typecheck (typecheck)
import System.Exit as X (ExitCode (..))
import System.IO as X (hPutStrLn)
import qualified Text.Megaparsec as Mega (errorBundlePretty)

interpretIO :: Text -> IO ()
interpretIO src = do
  val <- interpret src
  TIO.putStrLn (prettyValue val)

exit :: Natural -> [Char] -> IO void
exit status msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure (fromIntegral status))

interpret :: Text -> IO Value
interpret src =
  case parse src of
    Left e ->
      exit parseError (Mega.errorBundlePretty e)
    Right expr -> do
      case typecheck mempty expr of
        Left e ->
          exit typecheckError (show e)
        Right _ ->
          pure ()

      case eval mempty expr of
        Left e ->
          exit otherError (show e)
        Right val ->
          pure val
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
