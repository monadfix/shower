module Shower.Printer where

import Data.Coerce
import qualified Text.PrettyPrint as PP

import Shower.Class

newtype ShowerDoc = SD PP.Doc

instance Shower ShowerDoc where
  showerRecord = coerce showerRecord'
  showerList = coerce showerList'
  showerTuple = coerce showerTuple'
  showerStringLit = coerce showerStringLit'
  showerCharLit = coerce showerCharLit'
  showerSpace = coerce showerSpace'
  showerAtom = coerce showerAtom'

showerRecord' :: [(PP.Doc, PP.Doc)] -> PP.Doc
showerRecord' fields =
  PP.hsep [PP.lbrace, PP.nest 2 (showerFields fields), PP.rbrace]
  where
    showerFields = PP.sep . PP.punctuate PP.comma . map showerField
    showerField (name, x) = PP.hang (name PP.<+> PP.equals) 2 x

showerList' :: [PP.Doc] -> PP.Doc
showerList' elements =
  PP.hsep [PP.lbrack, PP.nest 2 (showerElements elements), PP.rbrack]
  where
    showerElements = PP.sep . PP.punctuate PP.comma

showerTuple' :: [PP.Doc] -> PP.Doc
showerTuple' elements =
  PP.hsep [PP.lparen, PP.nest 2 (showerElements elements), PP.rparen]
  where
    showerElements = PP.sep . PP.punctuate PP.comma

showerSpace' :: [PP.Doc] -> PP.Doc
showerSpace' (x:xs) = PP.hang x 2 (PP.sep xs)
showerSpace' xs = PP.sep xs

showerAtom' :: String -> PP.Doc
showerAtom' = PP.text

showerStringLit' :: String -> PP.Doc
showerStringLit' = PP.text . show

showerCharLit' :: Char -> PP.Doc
showerCharLit' = PP.text . show

showerRender :: ShowerDoc -> String
showerRender (SD showerDoc) =
  PP.renderStyle PP.style{ PP.lineLength = 80 } showerDoc ++ "\n"
