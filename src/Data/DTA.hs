{-# LANGUAGE DeriveDataTypeable #-}
{- | The Harmonix DTA/DTB metadata file format, used in the Karaoke Revolution,
     Guitar Hero, and Rock Band video game series. -}
module Data.DTA where

import Data.Int (Int32)
import Data.Word (Word8, Word32)
import qualified Data.Text as T

import Control.Monad
import Control.Monad.Trans.State

import Data.Typeable
import Data.Data

--
-- Basic types
--

-- | A top-level file.
data DTA = DTA { byte0 :: Word8, topTree :: Tree }
  deriving (Eq, Ord, Show, Read, Typeable, Data)

-- | A list of chunks, for either the top-level tree or a subtree.
data Tree = Tree { nodeID :: Word32, treeChunks :: [Chunk] }
  deriving (Eq, Ord, Show, Read, Typeable, Data)

{- | A data value, which may be a subtree. The constructors are ordered by their
     chunk identification tag in the binary format. -}
data Chunk
  = Int Int32
  | Float Float
  | Var T.Text
  | Key T.Text
  | Unhandled
  | IfDef T.Text
  | Else
  | EndIf
  | Parens Tree
  | Braces Tree
  | String T.Text
  | Brackets Tree
  | Define T.Text
  | Include T.Text
  | Merge T.Text
  | IfNDef T.Text
  deriving (Eq, Ord, Show, Read, Typeable, Data)

{- | Assign new sequential node IDs to each tree in a DTA, starting with the
     top-level tree. -}
renumberFrom :: Word32 -> DTA -> DTA
renumberFrom w (DTA b t) = DTA b $ evalState (renumberTree t) w where
  renumberTree :: Tree -> State Word32 Tree
  renumberTree (Tree _ sub) = liftM2 Tree get $
    modify (+1) >> mapM renumberChunk sub
  renumberChunk :: Chunk -> State Word32 Chunk
  renumberChunk c = case c of
    Parens tr -> fmap Parens $ renumberTree tr
    Braces tr -> fmap Braces $ renumberTree tr
    Brackets tr -> fmap Brackets $ renumberTree tr
    _ -> return c
  -- alternately, with uniplate: renumberChunk = descendBiM renumberTree
