-- | A @megaparsec@ implementation of a parser for 'Shower'.
module Shower.Parser (pShower) where

import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char

import Shower.Class

type Parser = Parsec Void String

pLexeme :: Parser a -> Parser a
pLexeme p = p <* space

-- | Parser for 'Shower' expressions.
pShower :: Shower a => Parsec Void String a
pShower = space *> pExpr

pExpr :: Shower a => Parser a
pExpr = showerSpace <$> some pPart

pCommaSep :: Parser a -> Parser [ShowerComma a]
pCommaSep p = many $
  ShowerCommaSep <$ pLexeme (char ',') <|>
  ShowerCommaElement <$> p

pPart :: Shower a => Parser a
pPart =
  pRecord <|>
  pList <|>
  pTuple <|>
  pStringLit <|>
  pCharLit <|>
  pAtom "()[]{},="

pRecord :: Shower a => Parser a
pRecord = do
  _ <- pLexeme (char '{')
  fields <- pCommaSep pField
  _ <- pLexeme (char '}')
  return (showerRecord fields)

pFieldName :: Shower a => Parser a
pFieldName =
  pStringLit <|>
  pAtom "()[]{},=:"

pField :: Shower a => Parser (a, ShowerFieldSep, a)
pField = do
  name <- pFieldName
  sep <- pLexeme $
    ShowerFieldSepEquals <$ char '=' <|>
    ShowerFieldSepColon  <$ char ':'
  value <- pExpr
  return (name, sep, value)

pList :: Shower a => Parser a
pList = do
  _ <- pLexeme (char '[')
  elements <- pCommaSep pExpr
  _ <- pLexeme (char ']')
  return (showerList elements)

pTuple :: Shower a => Parser a
pTuple = do
  _ <- pLexeme (char '(')
  elements <- pCommaSep pExpr
  _ <- pLexeme (char ')')
  return (showerTuple elements)

pQuotedLit :: Char -> Parser String
pQuotedLit quote =
  pLexeme $ do
    _ <- char quote
    s <- manyTill pSymbol (char quote)
    return (concat s)
  where
    pSymbol =
      string ['\\', '\\']  <|>
      string ['\\', quote] <|>
      ((:[]) <$> anySingle)

pStringLit :: Shower a => Parser a
pStringLit = showerStringLit <$> pQuotedLit '"'

pCharLit :: Shower a => Parser a
pCharLit = showerCharLit <$> pQuotedLit '\''

pAtom :: Shower a => [Char] -> Parser a
pAtom disallowed =
  pLexeme $ do
    s <- some (satisfy atomChar)
    return (showerAtom s)
  where
    atomChar c =
      not (c `elem` disallowed) &&
      not (isSpace c)
