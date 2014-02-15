{-# LANGUAGE DeriveDataTypeable #-}
module Data.DTA.Base
( DTA(..), Tree(..), Chunk(..)
, renumberFrom
) where

import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import Data.Data (Data)
import Data.Int (Int32)
import Data.Typeable (Typeable)
import Data.Word (Word32, Word8)

import qualified Control.Monad.Trans.State as S
import Data.Binary (Binary(..), Put, Get)
import Data.Binary.Get (getWord32le, getWord16le, getBytes, skip)
import Data.Binary.IEEE754 (putFloat32le, getFloat32le)
import Data.Binary.Put (putWord32le, putWord16le, putByteString)
import qualified Test.QuickCheck as QC

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
    0x2  -> Var <$> getLenStr
    0x5  -> Key <$> getLenStr
    0x6  -> skip 4 >> return Unhandled
    0x7  -> IfDef <$> getLenStr
    0x8  -> skip 4 >> return Else
    0x9  -> skip 4 >> return EndIf
    0x10 -> Parens <$> get
    0x11 -> Braces <$> get
    0x12 -> String <$> getLenStr
    0x13 -> Brackets <$> get
    0x20 -> Define <$> getLenStr
    0x21 -> Include <$> getLenStr
    0x22 -> Merge <$> getLenStr
    0x23 -> IfNDef <$> getLenStr
    _    -> fail $ "Unidentified DTB chunk with ID " ++ show cid

-- | DTB string format: 4-byte length, then a string in latin-1.
putLenStr :: B.ByteString -> Put
putLenStr b = putWord32le (fromIntegral $ B.length b) >> putByteString b

-- | DTB string format: 4-byte length, then a string in latin-1.
getLenStr :: Get B.ByteString
getLenStr = getWord32le >>= getBytes . fromIntegral

--
-- QuickCheck testing instances
--

arbByteString :: QC.Gen B.ByteString
arbByteString = B.pack <$> QC.arbitrary

-- | Generates a tree which may have subtrees up to the given depth.
arbTree :: Int -> QC.Gen Tree
arbTree n = liftA2 Tree QC.arbitrary $ QC.listOf $ arbChunk n

-- | Generates a chunk which may have subtrees up to the given depth.
arbChunk :: Int -> QC.Gen Chunk
arbChunk 0 = arbLeaf
arbChunk n = QC.frequency
  [ (1, QC.elements [Parens, Braces, Brackets] <*> arbTree (n - 1))
  , (10, arbLeaf) ]

-- | Generates a chunk which is not a subtree.
arbLeaf :: QC.Gen Chunk
arbLeaf = QC.oneof
  [ Int <$> QC.arbitrary
  , Float <$> QC.arbitrary
  --, Var <$> arbByteString
  , Key <$> arbByteString
  , return Unhandled
  --, IfDef <$> arbByteString
  , return Else
  , return EndIf
  , String <$> arbByteString
  --, Define <$> arbByteString
  --, Include <$> arbByteString
  --, Merge <$> arbByteString
  --, IfNDef <$> arbByteString
  ]

instance QC.Arbitrary DTA where
  arbitrary = liftA2 DTA QC.arbitrary QC.arbitrary

instance QC.Arbitrary Tree where
  arbitrary = QC.sized $ \n -> arbTree $ if n > 2 then 2 else n

instance QC.Arbitrary Chunk where
  arbitrary = QC.sized $ \n -> arbChunk $ if n > 2 then 2 else n

-- | Assign new sequential node IDs to each tree in a DTA, starting with the
-- top-level tree.
renumberFrom :: Word32 -> DTA -> DTA
renumberFrom w (DTA b t) = DTA b $ S.evalState (renumberTree t) w where
  renumberTree :: Tree -> S.State Word32 Tree
  renumberTree (Tree _ sub) = liftA2 Tree S.get $
    S.modify (+ 1) >> mapM renumberChunk sub
  renumberChunk :: Chunk -> S.State Word32 Chunk
  renumberChunk c = case c of
    Parens tr -> Parens <$> renumberTree tr
    Braces tr -> Braces <$> renumberTree tr
    Brackets tr -> Brackets <$> renumberTree tr
    _ -> return c
  -- alternately, with uniplate: renumberChunk = descendBiM renumberTree