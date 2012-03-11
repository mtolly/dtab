-- | Reading/writing binary DTB files, using Data.Binary.
module Data.DTA.Binary where

import Data.DTA
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754 (putFloat32le, getFloat32le)
import System.IO (Handle)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding

import Control.Monad (replicateM)
import Control.Applicative

-- | DTA format: a single unknown-purpose byte, followed by a tree.
putDTA :: DTA -> Put
putDTA (DTA b t) = put b >> putTree t

-- | Tree format: 2-byte length, 4-byte node ID, then each element in sequence.
putTree :: Tree -> Put
putTree (Tree nid chks) = do
  putWord16le (fromIntegral $ length chks)
  putWord32le nid
  mapM_ putChunk chks

{- | Chunk format: 4-byte chunk type identifier, followed by at least 4 bytes
     of chunk-specific data. -}
putChunk :: Chunk -> Put
putChunk c = case c of
  Int i       -> putWord32le 0x0  >> putWord32le (fromIntegral i)
  Float f     -> putWord32le 0x1  >> putFloat32le f
  Var t       -> putWord32le 0x2  >> putLenText t
  Key t       -> putWord32le 0x5  >> putLenText t
  Unhandled   -> putWord32le 0x6  >> putWord32le 0
  IfDef t     -> putWord32le 0x7  >> putLenText t
  Else        -> putWord32le 0x8  >> putWord32le 0
  EndIf       -> putWord32le 0x9  >> putWord32le 0
  Parens tr   -> putWord32le 0x10 >> putTree tr
  Braces tr   -> putWord32le 0x11 >> putTree tr
  String t    -> putWord32le 0x12 >> putLenText t
  Brackets tr -> putWord32le 0x13 >> putTree tr
  Define t    -> putWord32le 0x20 >> putLenText t
  Include t   -> putWord32le 0x21 >> putLenText t
  Merge t     -> putWord32le 0x22 >> putLenText t
  IfNDef t    -> putWord32le 0x23 >> putLenText t

-- | String format: 4-byte length, followed by the string.
putLenText :: T.Text -> Put
putLenText t = putWord32le (fromIntegral $ B.length b) >> putByteString b
  where b = encodeUtf8 t

-- | String format: 4-byte length, followed by the string.
getLenText :: Get T.Text
getLenText = fmap decodeUtf8 $ getWord32le >>= getBytes . fromIntegral

-- | Tree format: 2-byte length, 4-byte node ID, then each element in sequence.
getTree :: Get Tree
getTree = do
  len <- getWord16le
  liftA2 Tree getWord32le $ replicateM (fromIntegral len) getChunk

{- | Chunk format: 4-byte chunk type identifier, followed by at least 4 bytes
     of chunk-specific data. -}
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

-- | Read a DTB (binary) file.
readDTB :: FilePath -> IO DTA
readDTB = fmap (runGet getDTA) . BL.readFile

-- | Write a DTA file as DTB (binary).
writeDTB :: FilePath -> DTA -> IO ()
writeDTB fp = BL.writeFile fp . runPut . putDTA

-- | Read a DTB (binary) file from a handle.
hReadDTB :: Handle -> IO DTA
hReadDTB = fmap (runGet getDTA) . BL.hGetContents

-- | Write a DTA file as DTB (binary) to a handle.
hWriteDTB :: Handle -> DTA -> IO ()
hWriteDTB h = BL.hPutStr h . runPut . putDTA
