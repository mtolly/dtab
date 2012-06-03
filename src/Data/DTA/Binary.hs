-- | Reading/writing binary \".dtb\" files, using Data.Binary.
module Data.DTA.Binary where

import Data.DTA
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754 (putFloat32le, getFloat32le)
import System.IO (Handle)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Monad (replicateM)
import Control.Applicative

-- | Read a DTB (binary) file.
fromFile :: FilePath -> IO DTA
fromFile fp = runGet getDTA <$> BL.readFile fp

-- | Write a DTA file as DTB (binary).
toFile :: FilePath -> DTA -> IO ()
toFile fp dta = BL.writeFile fp $ runPut $ putDTA dta

-- | Read a DTB (binary) file from a handle.
fromHandle :: Handle -> IO DTA
fromHandle h = runGet getDTA <$> BL.hGetContents h

-- | Write a DTA file as DTB (binary) to a handle.
toHandle :: Handle -> DTA -> IO ()
toHandle h dta = BL.hPut h $ runPut $ putDTA dta

-- | DTA format: a single unknown-purpose byte, followed by a tree.
putDTA :: DTA -> Put
putDTA (DTA b t) = put b >> putTree t

-- | Tree format: 2-byte length, 4-byte node ID, then each element in sequence.
putTree :: Tree -> Put
putTree (Tree nid chks) = do
  putWord16le $ fromIntegral $ length chks
  putWord32le nid
  mapM_ putChunk chks

{- | Chunk format: 4-byte chunk type identifier, followed by at least 4 bytes
     of chunk-specific data. -}
putChunk :: Chunk -> Put
putChunk c = case c of
  Int i       -> putWord32le 0x0  >> putWord32le (fromIntegral i)
  Float f     -> putWord32le 0x1  >> putFloat32le f
  Var b       -> putWord32le 0x2  >> putLenStr b
  Key b       -> putWord32le 0x5  >> putLenStr b
  Unhandled   -> putWord32le 0x6  >> putWord32le 0
  IfDef b     -> putWord32le 0x7  >> putLenStr b
  Else        -> putWord32le 0x8  >> putWord32le 0
  EndIf       -> putWord32le 0x9  >> putWord32le 0
  Parens tr   -> putWord32le 0x10 >> putTree tr
  Braces tr   -> putWord32le 0x11 >> putTree tr
  String b    -> putWord32le 0x12 >> putLenStr b
  Brackets tr -> putWord32le 0x13 >> putTree tr
  Define b    -> putWord32le 0x20 >> putLenStr b
  Include b   -> putWord32le 0x21 >> putLenStr b
  Merge b     -> putWord32le 0x22 >> putLenStr b
  IfNDef b    -> putWord32le 0x23 >> putLenStr b

-- | String format: 4-byte length, followed by the string.
putLenStr :: B.ByteString -> Put
putLenStr b = do
  putWord32le $ fromIntegral $ B.length b
  putByteString b

-- | String format: 4-byte length, followed by the string.
getLenText :: Get B.ByteString
getLenText = getWord32le >>= getBytes . fromIntegral

-- | Tree format: 2-byte length, 4-byte node ID, then each element in sequence.
getTree :: Get Tree
getTree = do
  len <- getWord16le
  nid <- getWord32le
  Tree nid <$> replicateM (fromIntegral len) getChunk

-- | Chunk format: 4-byte chunk type identifier, followed by at least 4 bytes
-- of chunk-specific data.
getChunk :: Get Chunk
getChunk = getWord32le >>= \cid -> case cid of
  0x0  -> Int . fromIntegral <$> getWord32le
  0x1  -> Float <$> getFloat32le
  0x2  -> Var <$> getLenText
  0x5  -> Key <$> getLenText
  0x6  -> skip 4 >> return Unhandled
  0x7  -> IfDef <$> getLenText
  0x8  -> skip 4 >> return Else
  0x9  -> skip 4 >> return EndIf
  0x10 -> Parens <$> getTree
  0x11 -> Braces <$> getTree
  0x12 -> String <$> getLenText
  0x13 -> Brackets <$> getTree
  0x20 -> Define <$> getLenText
  0x21 -> Include <$> getLenText
  0x22 -> Merge <$> getLenText
  0x23 -> IfNDef <$> getLenText
  _    -> fail $ "Unidentified DTB chunk with ID " ++ show cid

-- | DTA format: a single unknown-purpose byte, followed by a tree.
getDTA :: Get DTA
getDTA = liftA2 DTA getWord8 getTree
