-- So that we can use parseTest in doctests without complaint:
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Based off my other project's parser (also MIT licensed):
--
--     https://github.com/seagreen/bowtie/blob/ffe75c43bc235c9f0dc8f4949295eb9727e9cdde/bowtie/src/Bowtie/Surface/Parse.hs
module PCF.Parse
  ( Parser
  , ParserErrorBundle
  , parse
  ) where

import Control.Applicative.Combinators.NonEmpty
import Data.Functor
import PCF.Expr
import PCF.Prelude hiding (many, some)

-- Hide @sepBy1@ because we're using the one from
-- @Control.Applicative.Combinators.NonEmpty@
-- that returns a @NonEmpty@ list instead.
import Text.Megaparsec hiding
  (State, Token, parse, parseTest, runParser, sepBy1, some)

import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Prelude
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

type ParserErrorBundle = ParseErrorBundle Text Void

parse :: Text -> Either ParserErrorBundle Expr
parse =
  runParser exprParser "<input>"

-- | Requires the parser to consume all input (unlike 'Mega.runParser').
runParser
  :: forall a. Parser a
  -> FilePath
  -> Text
  -> Either (ParseErrorBundle Text Void) a
runParser parser path =
  Mega.runParser f path
  where
    f :: Parser a
    f = do
      res <- parser
      Mega.eof
      pure res

-------------------------------------------------------------------------------
-- * exprParser

exprParser :: Parser Expr
exprParser =
  label "exprParser"
    (   lamParser
    <|> letParser
    <|> ifThenElseParser
    <|> valOrAppParser
    )

-- |
-- >>> parseTest lamParser "\\x : Nat. x"
-- Lam "x" NatType (Var "x")
lamParser :: Parser Expr
lamParser = do
  symbol "\\"
  id <- lexeme lowerIdParser
  typ <- annotationParser
  symbol "."
  expr <- exprParser
  pure (Lam id typ expr)
  where
    annotationParser :: Parser Type
    annotationParser = do
      symbol ":"
      typeParser

letParser :: Parser Expr
letParser = do
  symbol "let"
  id <- lexeme lowerIdParser
  symbol "="
  e1 <- exprParser
  symbol "in"
  e2 <- exprParser
  pure (Let id e1 e2)

ifThenElseParser :: Parser Expr
ifThenElseParser = do
  symbol "if"
  bExpr <- lexeme valOrAppParser
  symbol "then"
  e1 <- lexeme exprParser
  symbol "else"
  e2 <- lexeme exprParser
  pure (IfThenElse bExpr e1 e2)

-- |
-- >>> parseTest valOrAppParser "double 1"
-- App (Var "double") (NatLit 1)
valOrAppParser :: Parser Expr
valOrAppParser = do
  xs <- some argableExprParser
  let
    func :: Expr
    func =
      NonEmpty.head xs

    args :: [Expr]
    args =
      NonEmpty.tail xs

  pure (foldl' App func args) -- foldl because App is left associative

-------------------------------------------------------------------------------
-- * argableExprParser

-- | Parse an expression which could be a function argument.
--
-- This excludes let expressions, if-then-else, and lambdas
-- (unless they occur inside parentheses).
argableExprParser :: Parser Expr
argableExprParser =
  label "argableExprParser"
    (   lexeme (parens exprParser)
    <|> lexeme fixParser
    <|> symbol "true" $> BoolLit True
    <|> symbol "false" $> BoolLit False
    <|> symbol "suc" $> Suc
    <|> symbol "pred" $> Pred
    <|> symbol "is-zero" $> IsZero
    <|> lexeme varParser
    <|> lexeme intParser
    )

fixParser :: Parser Expr
fixParser = do
  symbol "fix"
  expr <- valOrAppParser -- TODO: this seems wrong
  pure (Fix expr)

varParser :: Parser Expr
varParser =
  fmap Var lowerIdParser

-- |
-- >>> parseTest lowerIdParser "a"
-- "a"
--
-- When we had a separate lexer this could just be tried after trying
-- to lex keyword tokens like "let" and "in". Now that we don't
-- it needs logic so that it doesn't eat those keywords.
lowerIdParser :: Parser Text
lowerIdParser = do
  notFollowedBy (keyword *> satisfy (not . validIdChar))
  c <- satisfy Char.isLower
  rest <- takeWhileP (Just "followup identifier char") validIdChar
  pure (Text.cons c rest)
  where
    keyword :: Parser ()
    keyword =
      void (asum (fmap chunk keywordList))

    keywordList :: [Text]
    keywordList =
      [ "let"
      , "in"
      , "fix"
      , "if"
      , "then"
      , "else"
      , "true"
      , "false"
      , "suc"
      , "pred"
      , "is-zero"
      ]

    validIdChar :: Char -> Bool
    validIdChar c =
      Char.isAlphaNum c || c == '-' || c == '_'

-- |
-- >>> parseTest intParser "1"
-- NatLit 1
intParser :: Parser Expr
intParser = do
  digits <- some (satisfy Char.isDigit)
  pure (NatLit (Prelude.read (NonEmpty.toList digits))) -- TODO: read

-------------------------------------------------------------------------------
-- * typeParser

-- |
-- >>> parseTest typeParser "Nat -> Nat"
-- ArrowType NatType NatType
typeParser :: Parser Type
typeParser = do
  xs <- typeStarParser `sepBy1` symbol "->"
  -- foldr because -> is right associative:
  pure (foldr ArrowType (NonEmpty.last xs) (NonEmpty.init xs))

-- | Parser for the part of a type definition separated by ->
-- in function definitions.
--
-- >>> parseTest typeStarParser "Nat"
-- NatType
typeStarParser :: Parser Type
typeStarParser =
  -- TODO: a good label
      symbol "Bool" $> BoolType
  <|> symbol "Nat" $> NatType

-------------------------------------------------------------------------------
-- * helpers

parens :: Parser a -> Parser a
parens =
  between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme p = do
  -- This is an inlined an modified Text.Megaparsec.Lexer.lexeme,
  -- which is defined as:
  -- lexeme spc p = p <* spc
  res <- p
  spacesOrNewlines
  pure res

symbol :: Text -> Parser ()
symbol =
  -- This is an inlined an modified Text.Megaparsec.Lexer.symbol,
  -- which is defined as:
  -- symbol spc = lexeme spc . string
  void . lexeme . chunk

spacesOrNewlines :: Parser ()
spacesOrNewlines =
  -- This is an inlined an modified Text.Megaparsec.Lexer.space.
  hidden (skipMany spaceOrNewline1)
  where
    spaceOrNewline1 :: Parser ()
    spaceOrNewline1 =
      void
        (takeWhile1P
          (Just "space or newline (U+0020 or U+000A)")
          (\c -> c == ' ' || c == '\n'))
