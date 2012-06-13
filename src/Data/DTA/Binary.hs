-- | Read/write binary (DTB) files, using Data.Binary.
-- 'DTA', 'Tree', and 'Chunk' all have 'Binary' instances.
-- All numeric values are little-endian.
module Data.DTA.Binary
( fromFile, toFile
, fromHandle, toHandle
) where

import Data.DTA
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754 (putFloat32le, getFloat32le)
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.Monad (replicateM)
import Control.Applicative

-- | Reads a strict ByteString from the file.
fromFile :: FilePath -> IO DTA
fromFile fp = withFile fp ReadMode fromHandle

toFile :: FilePath -> DTA -> IO ()
toFile fp dta = withFile fp WriteMode $ \h -> toHandle h dta

-- | Reads a strict ByteString from the handle.
fromHandle :: Handle -> IO DTA
fromHandle h = runGet get . lazyToStrict <$> B.hGetContents h
  where lazyToStrict l = BL.fromChunks [l]

toHandle :: Handle -> DTA -> IO ()
toHandle h dta = BL.hPut h $ runPut $ put dta

-- Single byte, then a tree.
instance Binary DTA where
  put (DTA b t) = put b >> put t
  get = liftA2 DTA get get

-- 2-byte length, 4-byte node ID, then each element in sequence.
instance Binary Tree where
  put (Tree nid chks) = do
    putWord16le $ fromIntegral $ length chks
    putWord32le nid
    mapM_ put chks
  get = do
    len <- getWord16le
    liftA2 Tree getWord32le $ replicateM (fromIntegral len) get

-- 4-byte chunk type ID, then at least 4 bytes of chunk data.
instance Binary Chunk where
  put c = case c of
    Int i       -> putWord32le 0x0  >> putWord32le (fromIntegral i)
    Float f     -> putWord32le 0x1  >> putFloat32le f
    Var b       -> putWord32le 0x2  >> putLenStr b
    Key b       -> putWord32le 0x5  >> putLenStr b
    Unhandled   -> putWord32le 0x6  >> putWord32le 0
    IfDef b     -> putWord32le 0x7  >> putLenStr b
    Else        -> putWord32le 0x8  >> putWord32le 0
    EndIf       -> putWord32le 0x9  >> putWord32le 0
    Parens tr   -> putWord32le 0x10 >> put tr
    Braces tr   -> putWord32le 0x11 >> put tr
    String b    -> putWord32le 0x12 >> putLenStr b
    Brackets tr -> putWord32le 0x13 >> put tr
    Define b    -> putWord32le 0x20 >> putLenStr b
    Include b   -> putWord32le 0x21 >> putLenStr b
    Merge b     -> putWord32le 0x22 >> putLenStr b
    IfNDef b    -> putWord32le 0x23 >> putLenStr b
  get = getWord32le >>= \cid -> case cid of
    0x0  -> Int . fromIntegral <$> getWord32le
    0x1  -> Float <$> getFloat32le
    0x2  -> Var <$> getLenText
    0x5  -> Key <$> getLenText
    0x6  -> skip 4 >> return Unhandled
    0x7  -> IfDef <$> getLenText
    0x8  -> skip 4 >> return Else
    0x9  -> skip 4 >> return EndIf
    0x10 -> Parens <$> get
    0x11 -> Braces <$> get
    0x12 -> String <$> getLenText
    0x13 -> Brackets <$> get
    0x20 -> Define <$> getLenText
    0x21 -> Include <$> getLenText
    0x22 -> Merge <$> getLenText
    0x23 -> IfNDef <$> getLenText
    _    -> fail $ "Unidentified DTB chunk with ID " ++ show cid

-- | String format: 4-byte length, followed by the string.
putLenStr :: B.ByteString -> Put
putLenStr b = do
  putWord32le $ fromIntegral $ B.length b
  putByteString b

-- | String format: 4-byte length, followed by the string.
getLenText :: Get B.ByteString
getLenText = getWord32le >>= getBytes . fromIntegral
