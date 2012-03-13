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
import qualified Data.Text as T

import Control.Monad (replicateM)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Applicative

-- | A wrapper to decode strings inside binary files.
type DecodeGet a = ReaderT (B.ByteString -> T.Text) Get a

-- | A wrapper to encode strings to binary files.
type EncodePut = ReaderT (T.Text -> B.ByteString) PutM ()

-- | Read a DTB (binary) file.
readDTB :: (B.ByteString -> T.Text) -> FilePath -> IO DTA
readDTB dec fp = runGet (runReaderT getDTA dec) <$> BL.readFile fp

-- | Write a DTA file as DTB (binary).
writeDTB :: (T.Text -> B.ByteString) -> FilePath -> DTA -> IO ()
writeDTB enc fp dta = BL.writeFile fp $ runPut $ runReaderT (putDTA dta) enc

-- | Read a DTB (binary) file from a handle.
hReadDTB :: (B.ByteString -> T.Text) -> Handle -> IO DTA
hReadDTB dec h = runGet (runReaderT getDTA dec) <$> BL.hGetContents h

-- | Write a DTA file as DTB (binary) to a handle.
hWriteDTB :: (T.Text -> B.ByteString) -> Handle -> DTA -> IO ()
hWriteDTB enc h dta = BL.hPutStr h $ runPut $ runReaderT (putDTA dta) enc

-- | DTA format: a single unknown-purpose byte, followed by a tree.
putDTA :: DTA -> EncodePut
putDTA (DTA b t) = lift (put b) >> putTree t

-- | Tree format: 2-byte length, 4-byte node ID, then each element in sequence.
putTree :: Tree -> EncodePut
putTree (Tree nid chks) = do
  lift $ putWord16le $ fromIntegral $ length chks
  lift $ putWord32le nid
  mapM_ putChunk chks

{- | Chunk format: 4-byte chunk type identifier, followed by at least 4 bytes
     of chunk-specific data. -}
putChunk :: Chunk -> EncodePut
putChunk c = case c of
  Int i       -> lift $ putWord32le 0x0  >> putWord32le (fromIntegral i)
  Float f     -> lift $ putWord32le 0x1  >> putFloat32le f
  Var t       -> lift (putWord32le 0x2) >> putLenText t
  Key t       -> lift (putWord32le 0x5)  >> putLenText t
  Unhandled   -> lift $ putWord32le 0x6  >> putWord32le 0
  IfDef t     -> lift (putWord32le 0x7)  >> putLenText t
  Else        -> lift $ putWord32le 0x8  >> putWord32le 0
  EndIf       -> lift $ putWord32le 0x9  >> putWord32le 0
  Parens tr   -> lift (putWord32le 0x10) >> putTree tr
  Braces tr   -> lift (putWord32le 0x11) >> putTree tr
  String t    -> lift (putWord32le 0x12) >> putLenText t
  Brackets tr -> lift (putWord32le 0x13) >> putTree tr
  Define t    -> lift (putWord32le 0x20) >> putLenText t
  Include t   -> lift (putWord32le 0x21) >> putLenText t
  Merge t     -> lift (putWord32le 0x22) >> putLenText t
  IfNDef t    -> lift (putWord32le 0x23) >> putLenText t

-- | String format: 4-byte length, followed by the string.
putLenText :: T.Text -> EncodePut
putLenText t = do
  enc <- ask
  let b = enc t
  lift $ putWord32le (fromIntegral $ B.length b) >> putByteString b

-- | String format: 4-byte length, followed by the string.
getLenText :: DecodeGet T.Text
getLenText = do
  dec <- ask
  fmap dec $ lift $ getWord32le >>= getBytes . fromIntegral

-- | Tree format: 2-byte length, 4-byte node ID, then each element in sequence.
getTree :: DecodeGet Tree
getTree = do
  len <- lift getWord16le
  liftA2 Tree (lift getWord32le) $ replicateM (fromIntegral len) getChunk

{- | Chunk format: 4-byte chunk type identifier, followed by at least 4 bytes
     of chunk-specific data. -}
getChunk :: DecodeGet Chunk
getChunk = lift getWord32le >>= \cid -> case cid of
  0x0  -> lift $ Int . fromIntegral <$> getWord32le
  0x1  -> lift $ Float <$> getFloat32le
  0x2  -> Var <$> getLenText
  0x5  -> Key <$> getLenText
  0x6  -> lift $ skip 4 >> return Unhandled
  0x7  -> IfDef <$> getLenText
  0x8  -> lift $ skip 4 >> return Else
  0x9  -> lift $ skip 4 >> return EndIf
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
getDTA :: DecodeGet DTA
getDTA = liftA2 DTA (lift getWord8) getTree
