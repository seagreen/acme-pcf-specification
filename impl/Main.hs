module Main where

import qualified Data.Text.IO as TIO
import Options.Applicative
import Pcf.Interpret (interpretIO)
import Pcf.Prelude

main :: IO ()
main = do
  checkForHelpFlag
  interpretIO =<< TIO.getContents

checkForHelpFlag :: IO ()
checkForHelpFlag =
  customExecParser (prefs showHelpOnError) configParser

configParser :: ParserInfo ()
configParser =
  info
    (helper <*> pure ())
    (progDesc "Interpret PCF source from STDIN")
