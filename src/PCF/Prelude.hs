{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
-- Normally we use hlint to enforce importing Data.Text as Text,
-- but here we want to import it as X:
{-# HLINT ignore "Avoid restricted qualification" #-}

-- | Tweak the @Prelude@. Includes no domain logic.
module PCF.Prelude
  ( module PCF.Prelude
  , module X
  ) where

-- Re-exports:

import Prelude as X hiding (error, foldl, id, lookup)

import Control.Applicative as X
import Control.Monad as X
import Data.Either as X
import Data.Foldable as X
import Data.Maybe as X
import Data.Text.Encoding as X hiding (decodeUtf8)
import Data.Traversable as X
import Data.Void as X
import Debug.Trace as X

import Data.ByteString as X (ByteString)
import Data.HashMap.Strict as X (HashMap)
import Data.Set as X (Set)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)

-- Local stuff:

import GHC.Stack.Types (HasCallStack)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Error (ioError, userError)
import Text.Megaparsec (ShowErrorComponent, Stream, Parsec, eof)

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Prelude
import qualified Text.Megaparsec as Mega (parseTest)

identity :: a -> a
identity a =
  a

{-# WARNING error "'error' remains in code" #-}
error :: HasCallStack => [Char] -> a
error =
  Prelude.error

panic :: HasCallStack => Text -> a
panic =
  error . Text.unpack

throwText :: Text -> IO a
throwText =
  ioError . userError . Text.unpack

exitWithError :: Text -> IO a
exitWithError e = do
  TIO.hPutStrLn stderr e
  exitFailure

-- | For doctests.
-- Requires the parser to consume all input (unlike 'Mega.parseTest').
--
-- Can't be put in 'PCF.Parse' because that gives an @unused-top-binds@
-- error, and I don't want to disable that error module-wide.
parseTest
  :: (ShowErrorComponent e, Show a, Stream s)
  => Parsec e s a
  -> s
  -> IO ()
parseTest p =
  Mega.parseTest do
    res <- p
    eof
    pure res
