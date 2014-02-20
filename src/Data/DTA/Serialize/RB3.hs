{-# LANGUAGE OverloadedStrings #-}
module Data.DTA.Serialize.RB3 where

import Data.DTA
import Data.DTA.Serialize
import Data.DTA.Serialize.Magma (Gender(..))
import qualified Data.ByteString.Char8 as B8
import Control.Applicative
import qualified Data.Map as Map
import Control.Monad ((>=>))

data SongPackage = SongPackage
  { name :: B8.ByteString
  , artist :: B8.ByteString
  , master :: Bool
  , song :: Song
  , songScrollSpeed :: Integer
  , bank :: B8.ByteString
  , animTempo :: Integer
  , songLength :: Integer
  , preview :: (Integer, Integer)
  , rank :: Rank
  , genre :: Keyword
  , vocalGender :: Gender
  , version :: Integer
  , format :: Integer
  , albumArt :: Bool
  , yearReleased :: Integer
  , rating :: Integer
  , subGenre :: Keyword
  , songID :: Either Integer B8.ByteString
  , tuningOffsetCents :: Float
  , guidePitchVolume :: Float
  , gameOrigin :: Keyword
  , encoding :: Keyword
  , albumName :: B8.ByteString
  , albumTrackNumber :: Integer
  } deriving (Eq, Ord, Show, Read)

data Song = Song
  { songName :: B8.ByteString
  , tracksCount :: [Integer]
  , tracks :: Tracks
  , pans :: [Float]
  , vols :: [Float]
  , cores :: [Integer]
  , vocalParts :: Integer
  , drumSolo :: [B8.ByteString]
  , drumFreestyle :: [B8.ByteString]
  } deriving (Eq, Ord, Show, Read)

data Tracks = Tracks
  { tracksDrum :: [Integer]
  } deriving (Eq, Ord, Show, Read)

data Rank = Rank
  { rankDrum :: Integer
  , rankGuitar :: Integer
  , rankBass :: Integer
  , rankVocals :: Integer
  , rankKeys :: Integer
  , rankRealKeys :: Integer
  , rankRealGuitar :: Integer
  , rankRealBass :: Integer
  , rankBand :: Integer
  } deriving (Eq, Ord, Show, Read)
