-- | Showing DTA files as text, using the HughesPJ pretty printer.
module Data.DTA.PrettyPrint where

import Data.DTA
import Text.PrettyPrint.HughesPJ
import System.IO
import qualified Data.Text as T
import qualified Data.ByteString as B

ppChunk :: Chunk -> Doc
ppChunk c = case c of
  Int i -> text $ show i
  Float f -> text $ show f
  Var t -> hcat [char '$', ppText t]
  Key t -> ppKey $ T.unpack t
  Unhandled -> text "kDataUnhandled"
  IfDef t -> hsep [text "#ifdef", ppText t]
  Else -> text "#else"
  EndIf -> text "#endif"
  Parens tr -> parens $ ppTree tr
  Braces tr -> braces $ ppTree tr
  String t -> text $ show $ T.unpack t
  Brackets tr -> brackets $ ppTree tr
  Define t -> hsep [text "#define", ppText t]
  Include t -> hsep [text "#include", ppText t]
  Merge t -> hsep [text "#merge", ppText t]
  IfNDef t -> hsep [text "#ifndef", ppText t]
  where ppText = text . T.unpack

{- | If the tree has more than 2 elements, all elements after the first get
     their own indented lines. -}
ppTree :: Tree -> Doc
ppTree (Tree _ chks) = case chks of
  (x : xt@(_ : _ : _)) -> vcat [ppChunk x, nest 2 $ vcat $ map ppChunk xt]
  _ -> hsep $ map ppChunk chks

ppKey :: String -> Doc
ppKey str = hcat [char '\'', hcat $ map ppChar str, char '\'']
  where ppChar '\'' = text "\\'"
        ppChar c    = char c

ppDTA :: DTA -> Doc
ppDTA = vcat . map ppChunk . treeChunks . topTree

writeDTA :: (T.Text -> B.ByteString) -> FilePath -> DTA -> IO ()
writeDTA enc fp = B.writeFile fp . enc . T.pack . render . ppDTA

hWriteDTA :: (T.Text -> B.ByteString) -> Handle -> DTA -> IO ()
hWriteDTA enc h = B.hPutStr h . enc . T.pack . render . ppDTA
