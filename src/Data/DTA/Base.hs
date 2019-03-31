{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.DTA.Base
( DTA(..), Tree(..), Chunk(..)
, renumberFrom
, binaryDTA, DTAVersion(..)
) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative       ((<$>))
#endif
import           Control.Applicative       (liftA2)
import           Control.Monad             (replicateM)
import qualified Data.ByteString           as B
import           Data.Data                 (Data)
import           Data.Int                  (Int32)
import           Data.Typeable             (Typeable)
import           Data.Word                 (Word32, Word8)

import qualified Control.Monad.Trans.State as S
import           Data.Binary               (Binary (..), Get, Put)
import           Data.Binary.Get           (getByteString, getWord16le,
                                            getWord32le, skip)
import           Data.Binary.IEEE754       (getFloat32le, putFloat32le)
import           Data.Binary.Put           (putByteString, putWord16le,
                                            putWord32le)

--
-- Type definitions
--

-- | A top-level file.
data DTA = DTA { byteZero :: Word8, topTree :: Tree }
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A list of chunks, for either the top-level tree or a subtree.
data Tree = Tree { nodeID :: Word32, treeChunks :: [Chunk] }
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A data value, which may be a subtree. The constructors are ordered by their
-- chunk identification tag in the binary format.
data Chunk
  = Int Int32
  | Float Float
  | Var B.ByteString
  | Key B.ByteString
  | Unhandled
  | IfDef B.ByteString
  | Else
  | EndIf
  | Parens Tree
  | Braces Tree
  | String B.ByteString
  | Brackets Tree
  | Define B.ByteString
  | Include B.ByteString
  | Merge B.ByteString
  | IfNDef B.ByteString
  deriving (Eq, Ord, Show, Read, Typeable, Data)

--
-- Binary (DTB) instances
--

data DTAVersion
  = DTAVersion1 -- ^ everything before and including RB3 AFAIK
  | DTAVersion2 -- ^ seen in Fantasia: Music Evolved

binaryDTA :: DTAVersion -> Get DTA
binaryDTA version = liftA2 DTA get (binaryTree version)

-- Single byte, then a tree.
instance Binary DTA where
  put (DTA b t) = put b >> put t
  get = binaryDTA DTAVersion1

binaryTree :: DTAVersion -> Get Tree
binaryTree version = case version of
  DTAVersion1 -> do
    len <- getWord16le
    nid <- getWord32le
    xs <- replicateM (fromIntegral len) (binaryChunk version)
    return $ Tree nid xs
  DTAVersion2 -> do
    _unk <- getWord32le -- always zero?
    len <- getWord32le
    nid <- fromIntegral <$> getWord16le
    xs <- replicateM (fromIntegral len) (binaryChunk version)
    return $ Tree nid xs

-- 2-byte length, 4-byte node ID, then each element in sequence.
instance Binary Tree where
  put (Tree nid chks) = do
    putWord16le $ fromIntegral $ length chks
    putWord32le nid
    mapM_ put chks
  get = binaryTree DTAVersion1

binaryChunk :: DTAVersion -> Get Chunk
binaryChunk version = getWord32le >>= \cid -> case cid of
  0x0  -> Int . fromIntegral <$> getWord32le
  0x1  -> Float <$> getFloat32le
  0x2  -> Var <$> getLenStr
  0x5  -> Key <$> getLenStr
  0x6  -> skip 4 >> return Unhandled
  0x7  -> IfDef <$> getLenStr
  0x8  -> skip 4 >> return Else
  0x9  -> skip 4 >> return EndIf
  0x10 -> Parens <$> binaryTree version
  0x11 -> Braces <$> binaryTree version
  0x12 -> String <$> getLenStr
  0x13 -> Brackets <$> binaryTree version
  0x20 -> Define <$> getLenStr
  0x21 -> Include <$> getLenStr
  0x22 -> Merge <$> getLenStr
  0x23 -> IfNDef <$> getLenStr
  _    -> fail $ "Unidentified DTB chunk with ID " ++ show cid

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
  get = binaryChunk DTAVersion1

-- | DTB string format: 4-byte length, then a string in latin-1.
putLenStr :: B.ByteString -> Put
putLenStr b = putWord32le (fromIntegral $ B.length b) >> putByteString b

-- | DTB string format: 4-byte length, then a string in latin-1.
getLenStr :: Get B.ByteString
getLenStr = getWord32le >>= getByteString . fromIntegral

-- | Assign new sequential node IDs to each tree in a DTA, starting with the
-- top-level tree.
renumberFrom :: Word32 -> DTA -> DTA
renumberFrom w (DTA b t) = DTA b $ S.evalState (renumberTree t) w where
  renumberTree :: Tree -> S.State Word32 Tree
  renumberTree (Tree _ sub) = liftA2 Tree S.get $
    S.modify (+ 1) >> mapM renumberChunk sub
  renumberChunk :: Chunk -> S.State Word32 Chunk
  renumberChunk c = case c of
    Parens tr   -> Parens <$> renumberTree tr
    Braces tr   -> Braces <$> renumberTree tr
    Brackets tr -> Brackets <$> renumberTree tr
    _           -> return c
  -- alternately, with uniplate: renumberChunk = descendBiM renumberTree
