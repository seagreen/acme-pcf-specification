-- | Tweak @Relude@. Includes no domain logic.
module Pcf.Prelude
  ( module Pcf.Prelude,
    module X,
  )
where

{- ORMOLU_DISABLE -}

-- Re-exports:

import Relude as X hiding (Type, error, id)

{- ORMOLU_ENABLE -}

-- Local stuff:

import qualified Relude as Relude
import Text.Megaparsec (Parsec, ShowErrorComponent, Stream, eof)
import qualified Text.Megaparsec as Mega (parseTest)

{-# WARNING error "'error' remains in code" #-}
error :: HasCallStack => Text -> a
error =
  Relude.error

panic :: HasCallStack => Text -> a
panic =
  Relude.error

-- | For doctests.
-- Requires the parser to consume all input (unlike 'Mega.parseTest').
--
-- Can't be put in 'Pcf.Parse' because that gives an @unused-top-binds@
-- error, and I don't want to disable that error module-wide.
parseTest ::
  (ShowErrorComponent e, Show a, Stream s) =>
  Parsec e s a ->
  s ->
  IO ()
parseTest p =
  Mega.parseTest do
    res <- p
    eof
    pure res
