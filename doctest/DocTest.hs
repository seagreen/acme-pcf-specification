module Main (main) where

import Prelude
import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  sourceFiles <- glob "src/**/*.hs"
  doctest
    -- NOTE: Keep in sync with package.yaml.
    $ "-XStrictData"
    : "-XBlockArguments"
    : "-XDeriveAnyClass"
    : "-XDeriveDataTypeable"
    : "-XDeriveFunctor"
    : "-XDeriveGeneric"
    : "-XDerivingStrategies"
    : "-XExistentialQuantification"
    : "-XFlexibleContexts"
    : "-XFlexibleInstances"
    : "-XFunctionalDependencies"
    : "-XGeneralizedNewtypeDeriving"
    : "-XInstanceSigs"
    : "-XLambdaCase"
    : "-XMultiParamTypeClasses"
    : "-XNoImplicitPrelude"
    : "-XOverloadedStrings"
    : "-XRankNTypes"
    : "-XScopedTypeVariables"
    : sourceFiles
