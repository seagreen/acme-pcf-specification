{-# LANGUAGE ApplicativeDo #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Options.Applicative hiding (command)
import Pcf.Eval (Value (..), prettyValue)
import Pcf.Prelude hiding (parseTest)
import qualified PcfTest.Eval as TE
import qualified PcfTest.Parse as TP
import qualified PcfTest.Suite as Suite
import qualified PcfTest.Typecheck as TT
import System.Exit as X (ExitCode (..))
import System.Process.Typed

data Config
  = -- | The implementation command being tested
    Config Text
  deriving (Eq, Show)

main :: IO ()
main = do
  Config command <- cliArgs
  putStrLn "# Parse tests"
  for_ (Suite.parseTests Suite.tests) (parseTest command)
  putStrLn "\n# Typecheck tests"
  for_ (Suite.typecheckTests Suite.tests) (typecheckTest command)
  putStrLn "\n# Evaluation tests"
  for_ (Suite.evalTests Suite.tests) (evalTest command)

-- * Helpers

log :: Text -> IO ()
log =
  TIO.hPutStr stderr

logLn :: Text -> IO ()
logLn =
  TIO.hPutStrLn stderr

-- * Runners

parseTest :: Text -> TP.TestCase -> IO ()
parseTest command TP.TestCase {TP.name, TP.shouldSucceed, TP.source} = do
  log ("+ " <> name)
  (exitCode, _stdoutTxt, stderrTxt) <- runTextCommand command source
  case (shouldSucceed, exitCode) of
    (True, ExitSuccess) ->
      logLn " <passed>"
    -- Some of the parse tests fail to typecheck, that's expected:
    (True, ExitFailure 3) ->
      logLn " <passed>"
    -- If we expect a parse test to fail, it must fail at the parsing stage:
    (False, ExitFailure 2) ->
      logLn " <passed>"
    _ ->
      die
        ( Text.unpack
            ( mconcat
                [ " <failed>\n\n",
                  if shouldSucceed
                    then "Should have succeeded\n\n"
                    else "Should have failed with exit code 2 (parse failure)\n\n",
                  "Test source:\n" <> source <> "\n\n",
                  "Exit status: " <> Text.pack (show exitCode) <> "\n\n",
                  "Stderr: " <> stderrTxt
                ]
            )
        )

typecheckTest :: Text -> TT.TestCase -> IO ()
typecheckTest command TT.TestCase {TT.name, TT.shouldSucceed, TT.source} = do
  log ("+ " <> name)
  (exitCode, _stdoutTxt, stderrTxt) <- runTextCommand command source
  case (shouldSucceed, exitCode) of
    (True, ExitSuccess) ->
      logLn " <passed>"
    -- If we expect a typechecking test to fail, it must fail at that stage:
    (False, ExitFailure 3) ->
      logLn " <passed>"
    _ ->
      die
        ( Text.unpack
            ( mconcat
                [ " <failed>\n\n",
                  if shouldSucceed
                    then "Should have succeeded\n\n"
                    else "Should have failed with exit code 3 (typecheck failure)\n\n",
                  "Test source:\n" <> source <> "\n\n",
                  "Exit status: " <> Text.pack (show exitCode) <> "\n\n",
                  "Stderr: " <> stderrTxt
                ]
            )
        )

evalTest :: Text -> TE.TestCase -> IO ()
evalTest command TE.TestCase {TE.name, TE.expected, TE.source} = do
  log ("+ " <> name)
  (exitCode, stdoutTxt, stderrTxt) <- runTextCommand command source
  case exitCode of
    ExitSuccess -> do
      let mExpectedTxt =
            case expected of
              TE.BoolExpected b ->
                Just (prettyValue (BoolVal b) <> "\n")
              TE.NatExpected n ->
                Just (prettyValue (NatVal n) <> "\n")
              TE.GenericSuccess ->
                Nothing

      case mExpectedTxt of
        Nothing ->
          pure ()
        Just expectedTxt ->
          when
            (stdoutTxt /= expectedTxt)
            ( die
                ( Text.unpack
                    ( mconcat
                        [ " <failed>\n\n",
                          "Test source:\n" <> source <> "\n\n",
                          "Expected (note that it ends with a newline):\n",
                          "```\n" <> expectedTxt <> "```\n",
                          "But got:\n",
                          "```\n" <> stdoutTxt <> "```\n",
                          "Stderr: " <> stderrTxt
                        ]
                    )
                )
            )

      logLn " <passed>"
    ExitFailure _ ->
      die
        ( Text.unpack
            ( mconcat
                [ " <failed>\n\n",
                  "Test source:\n" <> source <> "\n\n",
                  "Exit status: " <> Text.pack (show exitCode) <> "\n\n",
                  "Stderr: " <> stderrTxt
                ]
            )
        )

-- * CLI

cliArgs :: IO Config
cliArgs =
  customExecParser (prefs showHelpOnError) configParser

configParser :: ParserInfo Config
configParser =
  info
    (helper <*> parser)
    (progDesc "Test an implementation of Acme PCF")
  where
    parser :: Parser Config
    parser = do
      a <-
        argument
          str
          ( metavar "IMPL"
              <> help "Command to run the implementation being tested"
          )
      pure (Config a)

-- * Below should be in a lib somewhere

-- | NOTE: Only use with trused input!
runTextCommand ::
  -- | Command injection vulnerability when passed untrusted input.
  Text ->
  Text ->
  IO (ExitCode, Text, Text)
runTextCommand cmd input = do
  (exitCode, stdoutBts, stderrBts) <- runCommand cmd "" (encodeUtf8 input)
  pure
    ( exitCode,
      decodeUtf8 (LBS.toStrict stdoutBts), -- TODO: decodeUtf8
      decodeUtf8 (LBS.toStrict stderrBts)
    )

-- | NOTE: Only use with trused input!
runCommand ::
  -- | Command injection vulnerability when passed untrusted input.
  Text ->
  -- | Command injection vulnerability when passed untrusted input.
  Text ->
  ByteString ->
  IO (ExitCode, LBS.ByteString, LBS.ByteString)
runCommand cmd args input = do
  readProcess proc2
  where
    -- Command with argument
    proc1 :: ProcessConfig () () ()
    proc1 =
      shell (Text.unpack cmd <> " " <> Text.unpack args)

    -- Command with argument and stdin
    proc2 :: ProcessConfig () () ()
    proc2 =
      setStdin (byteStringInput (LBS.fromStrict input)) proc1
