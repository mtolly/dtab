{-# LANGUAGE DeriveDataTypeable #-}
-- | The basic tree types for DTA files.
module Data.DTA where

import Data.Int (Int32)
import Data.Word (Word8, Word32)

import Control.Monad.Trans.State
import Control.Applicative

import Data.Typeable
import Data.Data

import qualified Data.ByteString as B

-- | A top-level file.
data DTA = DTA { byte0 :: Word8, topTree :: Tree }
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

-- | Assign new sequential node IDs to each tree in a DTA, starting with the
-- top-level tree.
renumberFrom :: Word32 -> DTA -> DTA
renumberFrom w (DTA b t) = DTA b $ evalState (renumberTree t) w where
  renumberTree :: Tree -> State Word32 Tree
  renumberTree (Tree _ sub) = liftA2 Tree get $
    modify (+ 1) >> mapM renumberChunk sub
  renumberChunk :: Chunk -> State Word32 Chunk
  renumberChunk c = case c of
    Parens tr -> Parens <$> renumberTree tr
    Braces tr -> Braces <$> renumberTree tr
    Brackets tr -> Brackets <$> renumberTree tr
    _ -> return c
  -- alternately, with uniplate: renumberChunk = descendBiM renumberTree
