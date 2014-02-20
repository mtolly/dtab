{-# LANGUAGE OverloadedStrings #-}
module Data.DTA.Serialize.Magma where

import Data.DTA
import Data.DTA.Serialize
import qualified Data.ByteString.Char8 as B8
import Control.Applicative
import qualified Data.Map as Map
import Control.Monad ((>=>))

data Project = Project
  { toolVersion     :: B8.ByteString
  , projectVersion  :: Integer
  , metadata        :: Metadata
  , gamedata        :: Gamedata
  , languages       :: Languages
  , destinationFile :: B8.ByteString
  , midi            :: Midi
  , dryVox          :: DryVox
  , albumArtFile    :: B8.ByteString
  , tracks          :: Tracks
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Project where
  toChunks x = makeDict'
    [ ("tool_version"                 , toChunks $ toolVersion     x)
    , ("project_version"              , toChunks $ projectVersion  x)
    , ("metadata"                     , toChunks $ metadata        x)
    , ("gamedata"                     , toChunks $ gamedata        x)
    , ("languages"                    , toChunks $ languages       x)
    , ("destination_file"             , toChunks $ destinationFile x)
    , ("midi"                         , toChunks $ midi            x)
    , ("dry_vox"                      , toChunks $ dryVox          x)
    , ("album_art", makeDict' [("file", toChunks $ albumArtFile    x)])
    , ("tracks"                       , toChunks $ tracks          x)
    ]

instance FromChunks Project where
  fromChunks = getDict >=> \d -> Project
    <$> (dictLookup "tool_version"     d >>= fromChunks)
    <*> (dictLookup "project_version"  d >>= fromChunks)
    <*> (dictLookup "metadata"         d >>= fromChunks)
    <*> (dictLookup "gamedata"         d >>= fromChunks)
    <*> (dictLookup "languages"        d >>= fromChunks)
    <*> (dictLookup "destination_file" d >>= fromChunks)
    <*> (dictLookup "midi"             d >>= fromChunks)
    <*> (dictLookup "dry_vox"          d >>= fromChunks)
    <*> (dictLookup "album_art"        d >>=
      getDict >>= dictLookup "file"      >>= fromChunks)
    <*> (dictLookup "tracks"           d >>= fromChunks)

instance DTAFormat Project where
  serialize p = DTA 0 $ Tree 0 $ makeDict' [("project", toChunks p)]
  unserialize (DTA _ (Tree _ cs)) =
    getDict cs >>= dictLookup "project" >>= fromChunks

data Languages = Languages
  { english  :: Bool
  , french   :: Bool
  , italian  :: Bool
  , spanish  :: Bool
  , german   :: Bool
  , japanese :: Bool
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Languages where
  toChunks l = makeDict'
    [ ("english" , toChunks $ english  l)
    , ("french"  , toChunks $ french   l)
    , ("italian" , toChunks $ italian  l)
    , ("spanish" , toChunks $ spanish  l)
    , ("german"  , toChunks $ german   l)
    , ("japanese", toChunks $ japanese l)
    ]

instance FromChunks Languages where
  fromChunks = getDict >=> \d -> Languages
    <$> (dictLookup "english"  d >>= fromChunks)
    <*> (dictLookup "french"   d >>= fromChunks)
    <*> (dictLookup "italian"  d >>= fromChunks)
    <*> (dictLookup "spanish"  d >>= fromChunks)
    <*> (dictLookup "german"   d >>= fromChunks)
    <*> (dictLookup "japanese" d >>= fromChunks)

data Gamedata = Gamedata
  { previewStartMs   :: Integer
  , rankGuitar       :: Integer
  -- ^ 1 is no dots, 7 is devils.
  , rankBass         :: Integer
  , rankDrum         :: Integer
  , rankVocals       :: Integer
  , rankKeys         :: Integer
  , rankProKeys      :: Integer
  , rankBand         :: Integer
  , vocalScrollSpeed :: Integer
  -- ^ Normal = 2300.
  --   Fast = 2000.
  , animTempo        :: Integer
  -- ^ Slow (under 100bpm) = 16.
  --   Medium (100-160bpm) = 32.
  --   Fast (over 160bpm) = 64.
  , vocalGender      :: Gender
  , vocalPercussion  :: Percussion
  , vocalParts       :: Integer
  , guidePitchVolume :: Float
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Gamedata where
  toChunks gd = makeDict'
    [ ("preview_start_ms"  , toChunks $ previewStartMs   gd)
    , ("rank_guitar"       , toChunks $ rankGuitar       gd)
    , ("rank_bass"         , toChunks $ rankBass         gd)
    , ("rank_drum"         , toChunks $ rankDrum         gd)
    , ("rank_vocals"       , toChunks $ rankVocals       gd)
    , ("rank_keys"         , toChunks $ rankKeys         gd)
    , ("rank_pro_keys"     , toChunks $ rankProKeys      gd)
    , ("rank_band"         , toChunks $ rankBand         gd)
    , ("vocal_scroll_speed", toChunks $ vocalScrollSpeed gd)
    , ("anim_tempo"        , toChunks $ animTempo        gd)
    , ("vocal_gender"      , toChunks $ vocalGender      gd)
    , ("vocal_percussion"  , toChunks $ vocalPercussion  gd)
    , ("vocal_parts"       , toChunks $ vocalParts       gd)
    , ("guide_pitch_volume", toChunks $ guidePitchVolume gd)
    ]

instance FromChunks Gamedata where
  fromChunks = getDict >=> \d -> Gamedata
    <$> (dictLookup "preview_start_ms"   d >>= fromChunks)
    <*> (dictLookup "rank_guitar"        d >>= fromChunks)
    <*> (dictLookup "rank_bass"          d >>= fromChunks)
    <*> (dictLookup "rank_drum"          d >>= fromChunks)
    <*> (dictLookup "rank_vocals"        d >>= fromChunks)
    <*> (dictLookup "rank_keys"          d >>= fromChunks)
    <*> (dictLookup "rank_pro_keys"      d >>= fromChunks)
    <*> (dictLookup "rank_band"          d >>= fromChunks)
    <*> (dictLookup "vocal_scroll_speed" d >>= fromChunks)
    <*> (dictLookup "anim_tempo"         d >>= fromChunks)
    <*> (dictLookup "vocal_gender"       d >>= fromChunks)
    <*> (dictLookup "vocal_percussion"   d >>= fromChunks)
    <*> (dictLookup "vocal_parts"        d >>= fromChunks)
    <*> (dictLookup "guide_pitch_volume" d >>= fromChunks)

data Gender = Male | Female
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks Gender where
  toChunks Male   = [Key "male"]
  toChunks Female = [Key "female"]

instance FromChunks Gender where
  fromChunks [Key "male"  ] = Right Male
  fromChunks [Key "female"] = Right Female
  fromChunks cs = Left $ "Couldn't read as Gender: " ++ show cs

data Percussion = Tambourine | Cowbell | Handclap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks Percussion where
  toChunks Tambourine = [Key "tambourine"]
  toChunks Cowbell    = [Key "cowbell"]
  toChunks Handclap   = [Key "handclap"]

instance FromChunks Percussion where
  fromChunks [Key "tambourine"] = Right Tambourine
  fromChunks [Key "cowbell"   ] = Right Cowbell
  fromChunks [Key "handclap"  ] = Right Handclap
  fromChunks cs = Left $ "Couldn't read as Percussion: " ++ show cs

data Metadata = Metadata
  { songName     :: B8.ByteString
  , artistName   :: B8.ByteString
  , genre        :: Keyword
  , subGenre     :: Keyword
  , yearReleased :: Integer
  , albumName    :: B8.ByteString
  , author       :: B8.ByteString
  , releaseLabel :: B8.ByteString
  , country      :: Keyword
  , price        :: Integer
  , trackNumber  :: Integer
  , hasAlbum     :: Bool
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Metadata where
  toChunks md = makeDict'
    [ ("song_name"    , toChunks $ songName     md)
    , ("artist_name"  , toChunks $ artistName   md)
    , ("genre"        , toChunks $ genre        md)
    , ("sub_genre"    , toChunks $ subGenre     md)
    , ("year_released", toChunks $ yearReleased md)
    , ("album_name"   , toChunks $ albumName    md)
    , ("author"       , toChunks $ author       md)
    , ("release_label", toChunks $ releaseLabel md)
    , ("country"      , toChunks $ country      md)
    , ("price"        , toChunks $ price        md)
    , ("track_number" , toChunks $ trackNumber  md)
    , ("has_album"    , toChunks $ hasAlbum     md)
    ]

instance FromChunks Metadata where
  fromChunks = getDict >=> \d -> Metadata
    <$> (dictLookup "song_name"     d >>= fromChunks)
    <*> (dictLookup "artist_name"   d >>= fromChunks)
    <*> (dictLookup "genre"         d >>= fromChunks)
    <*> (dictLookup "sub_genre"     d >>= fromChunks)
    <*> (dictLookup "year_released" d >>= fromChunks)
    <*> (dictLookup "album_name"    d >>= fromChunks)
    <*> (dictLookup "author"        d >>= fromChunks)
    <*> (dictLookup "release_label" d >>= fromChunks)
    <*> (dictLookup "country"       d >>= fromChunks)
    <*> (dictLookup "price"         d >>= fromChunks)
    <*> (dictLookup "track_number"  d >>= fromChunks)
    <*> (dictLookup "has_album"     d >>= fromChunks)

data Midi = Midi
  { midiFile     :: B8.ByteString
  , autogenTheme :: B8.ByteString
  -- ^  \"\" (Default), \"AggressiveMetal.rbtheme\", \"ArenaRock.rbtheme\",
  -- \"DarkHeavyRock.rbtheme\", \"DustyVintage.rbtheme\", \"EdgyProgRock.rbtheme\",
  -- \"FeelGoodPopRock.rbtheme\", \"GaragePunkRock.rbtheme\",
  -- \"PsychJamRock.rbtheme\", \"SlowJam.rbtheme\", \"SynthPop.rbtheme\"
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Midi where
  toChunks m = makeDict'
    [ ("file"         , toChunks $ midiFile     m)
    , ("autogen_theme", toChunks $ autogenTheme m)
    ]

instance FromChunks Midi where
  fromChunks = getDict >=> \d -> Midi
    <$> (dictLookup "file"          d >>= fromChunks)
    <*> (dictLookup "autogen_theme" d >>= fromChunks)

data DryVox = DryVox
  { part0             :: DryVoxPart
  , part1             :: DryVoxPart
  , part2             :: DryVoxPart
  , tuningOffsetCents :: Float
  } deriving (Eq, Ord, Show, Read)

instance ToChunks DryVox where
  toChunks x = makeDict'
    [ ("part0"              , toChunks $ part0             x)
    , ("part1"              , toChunks $ part1             x)
    , ("part2"              , toChunks $ part2             x)
    , ("tuning_offset_cents", toChunks $ tuningOffsetCents x)
    ]

instance FromChunks DryVox where
  fromChunks = getDict >=> \d -> DryVox
    <$> (dictLookup "part0"               d >>= fromChunks)
    <*> (dictLookup "part1"               d >>= fromChunks)
    <*> (dictLookup "part2"               d >>= fromChunks)
    <*> (dictLookup "tuning_offset_cents" d >>= fromChunks)

data DryVoxPart = DryVoxPart
  { dryVoxFile    :: B8.ByteString
  , dryVoxEnabled :: Bool
  } deriving (Eq, Ord, Show, Read)

instance ToChunks DryVoxPart where
  toChunks x = makeDict'
    [ ("file"   , toChunks $ dryVoxFile    x)
    , ("enabled", toChunks $ dryVoxEnabled x)
    ]

instance FromChunks DryVoxPart where
  fromChunks = getDict >=> \d -> DryVoxPart
    <$> (dictLookup "file"    d >>= fromChunks)
    <*> (dictLookup "enabled" d >>= fromChunks)

data Tracks = Tracks
  { drumLayout :: DrumLayout
  , drumKit    :: AudioFile
  , drumKick   :: AudioFile
  , drumSnare  :: AudioFile
  , bass       :: AudioFile
  , guitar     :: AudioFile
  , vocals     :: AudioFile
  , keys       :: AudioFile
  , backing    :: AudioFile
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Tracks where
  toChunks x = makeDict'
    [ ("drum_layout", toChunks $ drumLayout x)
    , ("drum_kit"   , toChunks $ drumKit    x)
    , ("drum_kick"  , toChunks $ drumKick   x)
    , ("drum_snare" , toChunks $ drumSnare  x)
    , ("bass"       , toChunks $ bass       x)
    , ("guitar"     , toChunks $ guitar     x)
    , ("vocals"     , toChunks $ vocals     x)
    , ("keys"       , toChunks $ keys       x)
    , ("backing"    , toChunks $ backing    x)
    ]

instance FromChunks Tracks where
  fromChunks = getDict >=> \d -> Tracks
    <$> (dictLookup "drum_layout" d >>= fromChunks)
    <*> (dictLookup "drum_kit"    d >>= fromChunks)
    <*> (dictLookup "drum_kick"   d >>= fromChunks)
    <*> (dictLookup "drum_snare"  d >>= fromChunks)
    <*> (dictLookup "bass"        d >>= fromChunks)
    <*> (dictLookup "guitar"      d >>= fromChunks)
    <*> (dictLookup "vocals"      d >>= fromChunks)
    <*> (dictLookup "keys"        d >>= fromChunks)
    <*> (dictLookup "backing"     d >>= fromChunks)

data DrumLayout
  = Kit
  | KitSnare
  | KitKick
  | KitKickSnare
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks DrumLayout where
  toChunks Kit          = [Key "drum_layout_kit"           ]
  toChunks KitSnare     = [Key "drum_layout_kit_snare"     ]
  toChunks KitKick      = [Key "drum_layout_kit_kick"      ]
  toChunks KitKickSnare = [Key "drum_layout_kit_kick_snare"]

instance FromChunks DrumLayout where
  fromChunks [Key "drum_layout_kit"           ] = Right Kit
  fromChunks [Key "drum_layout_kit_snare"     ] = Right KitSnare
  fromChunks [Key "drum_layout_kit_kick"      ] = Right KitKick
  fromChunks [Key "drum_layout_kit_kick_snare"] = Right KitKickSnare
  fromChunks cs = Left $ "Couldn't read as DrumLayout: " ++ show cs

data AudioFile = AudioFile
  { audioEnabled :: Bool
  , channels     :: Integer
  , pan          :: [Float]
  , vol          :: [Float]
  , audioFile    :: B8.ByteString
  } deriving (Eq, Ord, Show, Read)

instance ToChunks AudioFile where
  toChunks x = makeDict'
    [ ("enabled" , toChunks $ audioEnabled x)
    , ("channels", toChunks $ channels     x)
    , ("pan"     , toChunks $ pan          x)
    , ("vol"     , toChunks $ vol          x)
    , ("file"    , toChunks $ audioFile    x)
    ]

instance FromChunks AudioFile where
  fromChunks = getDict >=> \d -> AudioFile
    <$> (dictLookup "enabled"  d >>= fromChunks)
    <*> (dictLookup "channels" d >>= fromChunks)
    <*> (dictLookup "pan"      d >>= fromChunks)
    <*> (dictLookup "vol"      d >>= fromChunks)
    <*> (dictLookup "file"     d >>= fromChunks)

dictLookup :: B8.ByteString -> Dict v -> Either String v
dictLookup k (Dict m) = case Map.lookup k m of
  Nothing -> Left $ "Couldn't find key " ++ show k
  Just v  -> Right v

makeDict' :: [(B8.ByteString, [Chunk])] -> [Chunk]
makeDict' = makeDict . Dict . Map.fromList
