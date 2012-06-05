-- | Showing DTA files as text, using the HughesPJ pretty printer.
module Data.DTA.PrettyPrint (toByteString, toString, toHandle, toFile) where

import Data.DTA
import Text.PrettyPrint.HughesPJ
import System.IO
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

-- | If the tree has more than 2 elements, all elements after the first get
-- their own indented lines.
ppTree :: Tree -> Doc
ppTree (Tree _ chks) = case chks of
  (x : xt@(_ : _ : _)) -> vcat [ppChunk x, nest 2 $ vcat $ map ppChunk xt]
  _ -> hsep $ map ppChunk chks

ppKey :: String -> Doc
ppKey str = text $ "'" ++ concatMap escape str ++ "'"
  where escape '\'' = "\\'"
        escape '\\' = "\\\\"
        escape c    = [c]

ppDTA :: DTA -> Doc
ppDTA = vcat . map ppChunk . treeChunks . topTree

toString :: DTA -> String
toString = render . ppDTA

toByteString :: DTA -> B8.ByteString
toByteString = B8.pack . toString

toHandle :: Handle -> DTA -> IO ()
toHandle h = B8.hPutStr h . toByteString

toFile :: FilePath -> DTA -> IO ()
toFile fp dta = withFile fp WriteMode $ \h -> toHandle h dta
