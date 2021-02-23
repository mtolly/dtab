-- | Pretty-print text (DTA) files with the HughesPJ library.
module Data.DTA.PrettyPrint (sToDTA) where

import qualified Data.ByteString.Char8     as B8
import           Data.Char                 (isAlphaNum)
import qualified Text.PrettyPrint.HughesPJ as PP

import           Data.DTA.Base

ppChunk :: Chunk -> PP.Doc
ppChunk c = case c of
  Int i -> PP.text $ show i
  Float f -> PP.text $ show f
  Var t -> PP.hcat [PP.char '$', ppText t]
  Sym t -> PP.text $ ppSym $ B8.unpack t
  Unhandled -> PP.text "kDataUnhandled"
  IfDef t -> PP.hsep [PP.text "#ifdef", ppText t]
  Else -> PP.text "#else"
  EndIf -> PP.text "#endif"
  Parens tr -> PP.parens $ ppTree tr
  Braces tr -> PP.braces $ ppTree tr
  String t -> PP.text $ doubleQuotedString $ B8.unpack t
  Brackets tr -> PP.brackets $ ppTree tr
  Define t -> PP.hsep [PP.text "#define", ppText t]
  Include t -> PP.hsep [PP.text "#include", ppText t]
  Merge t -> PP.hsep [PP.text "#merge", ppText t]
  IfNDef t -> PP.hsep [PP.text "#ifndef", ppText t]
  Autorun -> PP.text "#autorun"
  Undef t -> PP.hsep [PP.text "#undef", ppText t]
  where ppText = PP.text . B8.unpack

doubleQuotedString :: String -> String
doubleQuotedString t = let
  f '"'  = "\\q"
  f '\n' = "\\n"
  f '\\' = "\\\\"
  f ch   = [ch]
  in "\"" ++ concatMap f t ++ "\""

-- | Automatically chooses between horizontal and vertical arrangements,
-- depending on what kind of chunks are in the tree.
ppTree :: Tree -> PP.Doc
ppTree (Tree _ chks)
  | all simpleChunk chks = PP.hsep $ map ppChunk chks
  | otherwise            = PP.vcat $ map ppChunk chks
  where simpleChunk c = case c of
          Int _     -> True
          Float _   -> True
          Var _     -> True
          Sym _     -> True
          Unhandled -> True
          _         -> False

-- | Produces a raw symbol or single-quoted symbol literal.
ppSym :: String -> String
ppSym s
  | all (\c -> isAlphaNum c || elem c "_/.-=#<>&!") s && not (null s) = s
  | otherwise = let
    f '\'' = "\\q"
    f '\n' = "\\n"
    f '\\' = "\\\\"
    f ch   = [ch]
    in "'" <> concatMap f s <> "'"

ppDTA :: DTA -> PP.Doc
ppDTA = PP.vcat . map ppChunk . treeChunks . topTree

sToDTA :: DTA -> String
sToDTA = PP.render . ppDTA
