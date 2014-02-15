-- | Pretty-print text (DTA) files with the HughesPJ library.
module Data.DTA.PrettyPrint (sToDTA) where

import Data.DTA.Base
import Text.PrettyPrint.HughesPJ
import qualified Data.ByteString.Char8 as B8

ppChunk :: Chunk -> Doc
ppChunk c = case c of
  Int i -> text $ show i
  Float f -> text $ show f
  Var t -> hcat [char '$', ppText t]
  Key t -> ppKey $ B8.unpack t
  Unhandled -> text "kDataUnhandled"
  IfDef t -> hsep [text "#ifdef", ppText t]
  Else -> text "#else"
  EndIf -> text "#endif"
  Parens tr -> parens $ ppTree tr
  Braces tr -> braces $ ppTree tr
  String t -> text $ show $ B8.unpack t
  Brackets tr -> brackets $ ppTree tr
  Define t -> hsep [text "#define", ppText t]
  Include t -> hsep [text "#include", ppText t]
  Merge t -> hsep [text "#merge", ppText t]
  IfNDef t -> hsep [text "#ifndef", ppText t]
  where ppText = text . B8.unpack

-- | Automatically chooses between horizontal and vertical arrangements,
-- depending on what kind of chunks are in the tree.
ppTree :: Tree -> Doc
ppTree (Tree _ chks)
  | all simpleChunk chks = hsep $ map ppChunk chks
  | otherwise            = vcat $ map ppChunk chks
  where simpleChunk c = case c of
          Int _ -> True
          Float _ -> True
          Var _ -> True
          Key _ -> True
          Unhandled -> True
          _ -> False

-- | Produces a single-quoted string literal.
ppKey :: String -> Doc
ppKey = text . f . show where
  -- simply convert a double-quoted string to single-quoted string
  f "" = ""
  f ('"':xs) = '\'' : f xs
  f ('\'':xs) = '\\' : '\'' : f xs
  f ('\\':x:xs) = '\\' : x : f xs
  f (x:xs) = x : f xs

ppDTA :: DTA -> Doc
ppDTA = vcat . map ppChunk . treeChunks . topTree

sToDTA :: DTA -> String
sToDTA = render . ppDTA
