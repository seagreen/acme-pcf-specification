-- | Based off my other project's parser (also MIT licensed):
--
--     https://github.com/seagreen/bowtie/blob/ffe75c43bc235c9f0dc8f4949295eb9727e9cdde/bowtie/src/Bowtie/Surface/Parse.hs
module Pcf.Parse
  ( Parser,
    ParserErrorBundle,
    parse,
  )
where

-- Hide @sepBy1@ from 'Text.Megaparsec'because we're using
-- the one from @Control.Applicative.Combinators.NonEmpty@
-- that returns a @NonEmpty@ list instead.

import Control.Applicative.Combinators.NonEmpty
import qualified Data.Char as Char
import Data.Functor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Pcf.Expr
import Pcf.Prelude hiding (many, some)
import Text.Megaparsec hiding (parse, parseTest, sepBy1, some)
import qualified Prelude (read)

type Parser = Parsec Void Text

type ParserErrorBundle = ParseErrorBundle Text Void

parse :: Text -> Either ParserErrorBundle Expr
parse =
  runParser (exprParser <* eof) "<input>"

-------------------------------------------------------------------------------

-- * exprParser

exprParser :: Parser Expr
exprParser =
  label
    "exprParser"
    ( lamParser
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
  let func :: Expr
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
-- This excludes let expressions, if-then-else, lambdas,
-- and function application (unless they occur inside parentheses).
argableExprParser :: Parser Expr
argableExprParser =
  label
    "argableExprParser"
    ( lexeme (parens exprParser)
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
  expr <- argableExprParser
  pure (Fix expr)

varParser :: Parser Expr
varParser =
  fmap Var lowerIdParser

-- |
-- >>> parseTest lowerIdParser "abc"
-- "abc"
--
-- >>> parseMaybe lowerIdParser "let"
-- Nothing
--
-- >>> parseTest lowerIdParser "lett"
-- "lett"
lowerIdParser :: Parser Text
lowerIdParser = do
  notFollowedBy (keywordParser *> noFollowingIdChars)
  c <- satisfy Char.isLower
  rest <- takeWhileP (Just "followup identifier character") validIdChar
  pure (Text.cons c rest)
  where
    keywordParser :: Parser ()
    keywordParser =
      void (asum (fmap chunk keywordList))

    noFollowingIdChars :: Parser ()
    noFollowingIdChars =
      label
        "no following identifier characters after keyword"
        ( void (satisfy (not . validIdChar))
            <|> eof
        )

    keywordList :: [Text]
    keywordList =
      [ "let",
        "in",
        "fix",
        "if",
        "then",
        "else",
        "true",
        "false",
        "suc",
        "pred",
        "is-zero"
      ]

    validIdChar :: Char -> Bool
    validIdChar c =
      Char.isAlphaNum c || c == '-' || c == '_'

-- |
-- >>> parseTest intParser "123"
-- NatLit 123
intParser :: Parser Expr
intParser = do
  digits <- some (satisfy Char.isDigit)
  pure (NatLit (Prelude.read (NonEmpty.toList digits))) -- TODO: read

-------------------------------------------------------------------------------

-- * typeParser

-- |
-- >>> parseTest typeParser "(Nat -> Bool) -> Nat"
-- ArrowType (ArrowType NatType BoolType) NatType
typeParser :: Parser Type
typeParser = do
  xs <- typeStarParser `sepBy1` symbol "->"
  -- foldr because -> is right associative:
  pure (foldr ArrowType (NonEmpty.last xs) (NonEmpty.init xs))

-- | Parser for the part of a type signature separated by ->
--
-- >>> parseTest typeStarParser "Nat"
-- NatType
--
-- >>> parseTest typeStarParser "(Nat -> Bool)"
-- ArrowType NatType BoolType
typeStarParser :: Parser Type
typeStarParser =
  label
    "typeStarParser"
    ( parens typeParser
        <|> singleTypeParser
    )

-- |
-- >>> parseTest singleTypeParser "Nat"
-- NatType
singleTypeParser :: Parser Type
singleTypeParser =
  label
    "singleTypeParser"
    ( symbol "Bool" $> BoolType
        <|> symbol "Nat" $> NatType
    )

-------------------------------------------------------------------------------

-- * helpers

parens :: Parser a -> Parser a
parens =
  between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme p =
  -- An inlined Text.Megaparsec.Lexer.lexeme, which is defined as:
  -- lexeme spc p = p <* spc
  p <* spacesOrNewlines

symbol :: Text -> Parser ()
symbol =
  -- An inlined and modified Text.Megaparsec.Lexer.symbol,
  -- which is defined as:
  -- symbol spc = lexeme spc . string
  void . lexeme . chunk

spacesOrNewlines :: Parser ()
spacesOrNewlines =
  -- An inlined an modified Text.Megaparsec.Lexer.space.
  hidden (skipMany spaceOrNewline1)
  where
    spaceOrNewline1 :: Parser ()
    spaceOrNewline1 =
      void
        ( takeWhile1P
            (Just "space or newline (U+0020 or U+000A)")
            (\c -> c == ' ' || c == '\n')
        )
