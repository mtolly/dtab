{-# LANGUAGE OverloadedStrings #-}
module Data.DTA.Serialize.Magma where

import Data.DTA
import Data.DTA.Serialize
import qualified Data.ByteString.Char8 as B8
import Control.Applicative

data Languages = Languages
  { english  :: Bool
  , french   :: Bool
  , italian  :: Bool
  , spanish  :: Bool
  , german   :: Bool
  , japanese :: Bool
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Languages where
  toChunks l =
    [ tagged "english" $ toChunks $ english  l
    , tagged "french"  $ toChunks $ french   l
    , tagged "italian" $ toChunks $ italian  l
    , tagged "spanish" $ toChunks $ spanish  l
    , tagged "german"  $ toChunks $ german   l
    , tagged "japanese"$ toChunks $ japanese l
    ]

instance FromChunks Languages where
  fromChunks cs = Languages
    <$> (getTag "english"  cs >>= fromChunks)
    <*> (getTag "french"   cs >>= fromChunks)
    <*> (getTag "italian"  cs >>= fromChunks)
    <*> (getTag "spanish"  cs >>= fromChunks)
    <*> (getTag "german"   cs >>= fromChunks)
    <*> (getTag "japanese" cs >>= fromChunks)

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

data Gender = Male | Female
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Percussion = Tambourine | Cowbell | Handclap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks Gamedata where
  toChunks gd =
    [ tagged "preview_start_ms"   $ toChunks $ previewStartMs   gd
    , tagged "rank_guitar"        $ toChunks $ rankGuitar       gd
    , tagged "rank_bass"          $ toChunks $ rankBass         gd
    , tagged "rank_drum"          $ toChunks $ rankDrum         gd
    , tagged "rank_vocals"        $ toChunks $ rankVocals       gd
    , tagged "rank_keys"          $ toChunks $ rankKeys         gd
    , tagged "rank_pro_keys"      $ toChunks $ rankProKeys      gd
    , tagged "rank_band"          $ toChunks $ rankBand         gd
    , tagged "vocal_scroll_speed" $ toChunks $ vocalScrollSpeed gd
    , tagged "anim_tempo"         $ toChunks $ animTempo        gd
    , tagged "vocal_gender"       $ toChunks $ vocalGender      gd
    , tagged "vocal_percussion"   $ toChunks $ vocalPercussion  gd
    , tagged "vocal_parts"        $ toChunks $ vocalParts       gd
    , tagged "guide_pitch_volume" $ toChunks $ guidePitchVolume gd
    ]

instance ToChunks Gender where
  toChunks Male = [Key "male"]
  toChunks Female = [Key "female"]

instance ToChunks Percussion where
  toChunks Tambourine = [Key "tambourine"]
  toChunks Cowbell    = [Key "cowbell"]
  toChunks Handclap   = [Key "handclap"]

instance FromChunks Gender where
  fromChunks [Key "male"  ] = Right Male
  fromChunks [Key "female"] = Right Female
  fromChunks cs = Left $ "Couldn't read as Gender: " ++ show cs

instance FromChunks Percussion where
  fromChunks [Key "tambourine"] = Right Tambourine
  fromChunks [Key "cowbell"   ] = Right Cowbell
  fromChunks [Key "handclap"  ] = Right Handclap
  fromChunks cs = Left $ "Couldn't read as Percussion: " ++ show cs

instance FromChunks Gamedata where
  fromChunks cs = Gamedata
    <$> (getTag "preview_start_ms"   cs >>= fromChunks)
    <*> (getTag "rank_guitar"        cs >>= fromChunks)
    <*> (getTag "rank_bass"          cs >>= fromChunks)
    <*> (getTag "rank_drum"          cs >>= fromChunks)
    <*> (getTag "rank_vocals"        cs >>= fromChunks)
    <*> (getTag "rank_keys"          cs >>= fromChunks)
    <*> (getTag "rank_pro_keys"      cs >>= fromChunks)
    <*> (getTag "rank_band"          cs >>= fromChunks)
    <*> (getTag "vocal_scroll_speed" cs >>= fromChunks)
    <*> (getTag "anim_tempo"         cs >>= fromChunks)
    <*> (getTag "vocal_gender"       cs >>= fromChunks)
    <*> (getTag "vocal_percussion"   cs >>= fromChunks)
    <*> (getTag "vocal_parts"        cs >>= fromChunks)
    <*> (getTag "guide_pitch_volume" cs >>= fromChunks)

data Metadata = Metadata
  { songName     :: B8.ByteString
  , artistName   :: B8.ByteString
  , genre        :: Genre
  , subGenre     :: SubGenre
  , yearReleased :: Integer
  , albumName    :: B8.ByteString
  , author       :: B8.ByteString
  , releaseLabel :: B8.ByteString
  , country      :: Country
  , price        :: Integer
  , trackNumber  :: Integer
  , hasAlbum     :: Bool
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Metadata where
  toChunks md =
    [ tagged "song_name"      $ toChunks $ songName     md
    , tagged "artist_name"    $ toChunks $ artistName   md
    , tagged "genre"          $ toChunks $ genre        md
    , tagged "sub_genre"      $ toChunks $ subGenre     md
    , tagged "year_released"  $ toChunks $ yearReleased md
    , tagged "album_name"     $ toChunks $ albumName    md
    , tagged "author"         $ toChunks $ author       md
    , tagged "release_label"  $ toChunks $ releaseLabel md
    , tagged "country"        $ toChunks $ country      md
    , tagged "price"          $ toChunks $ price        md
    , tagged "track_number"   $ toChunks $ trackNumber  md
    , tagged "has_album"      $ toChunks $ hasAlbum     md
    ]

instance FromChunks Metadata where
  fromChunks cs = Metadata
    <$> (getTag "song_name"     cs >>= fromChunks)
    <*> (getTag "artist_name"   cs >>= fromChunks)
    <*> (getTag "genre"         cs >>= fromChunks)
    <*> (getTag "sub_genre"     cs >>= fromChunks)
    <*> (getTag "year_released" cs >>= fromChunks)
    <*> (getTag "album_name"    cs >>= fromChunks)
    <*> (getTag "author"        cs >>= fromChunks)
    <*> (getTag "release_label" cs >>= fromChunks)
    <*> (getTag "country"       cs >>= fromChunks)
    <*> (getTag "price"         cs >>= fromChunks)
    <*> (getTag "track_number"  cs >>= fromChunks)
    <*> (getTag "has_album"     cs >>= fromChunks)

data Genre = Metal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks Genre where
  toChunks Metal = [Key "metal"]

instance FromChunks Genre where
  fromChunks [Key "metal"] = Right Metal
  fromChunks cs = Left $ "Couldn't read as Genre: " ++ show cs

data SubGenre = Prog
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks SubGenre where
  toChunks Prog = [Key "subgenre_prog"]

instance FromChunks SubGenre where
  fromChunks [Key "subgenre_prog"] = Right Prog
  fromChunks cs = Left $ "Couldn't read as SubGenre: " ++ show cs

data Country
  = Australia
  | Canada
  | Denmark
  | France
  | Germany
  | Ireland
  | Italy
  | Japan
  | Netherlands
  | NewZealand
  | Norway
  | Singapore
  | Spain
  | Sweden
  | UnitedKingdom
  | UnitedStates
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks Country where
  toChunks Australia     = [Key "ugc_country_australia"]
  toChunks Canada        = [Key "ugc_country_canada"]
  toChunks Denmark       = [Key "ugc_country_denmark"]
  toChunks France        = [Key "ugc_country_france"]
  toChunks Germany       = [Key "ugc_country_germany"]
  toChunks Ireland       = [Key "ugc_country_ireland"]
  toChunks Italy         = [Key "ugc_country_italy"]
  toChunks Japan         = [Key "ugc_country_japan"]
  toChunks Netherlands   = [Key "ugc_country_netherlands"]
  toChunks NewZealand    = [Key "ugc_country_newzealand"]
  toChunks Norway        = [Key "ugc_country_norway"]
  toChunks Singapore     = [Key "ugc_country_singapore"]
  toChunks Spain         = [Key "ugc_country_spain"]
  toChunks Sweden        = [Key "ugc_country_sweden"]
  toChunks UnitedKingdom = [Key "ugc_country_uk"]
  toChunks UnitedStates  = [Key "ugc_country_us"]

instance FromChunks Country where
  fromChunks [Key "ugc_country_australia"  ] = Right Australia
  fromChunks [Key "ugc_country_canada"     ] = Right Canada
  fromChunks [Key "ugc_country_denmark"    ] = Right Denmark
  fromChunks [Key "ugc_country_france"     ] = Right France
  fromChunks [Key "ugc_country_germany"    ] = Right Germany
  fromChunks [Key "ugc_country_ireland"    ] = Right Ireland
  fromChunks [Key "ugc_country_italy"      ] = Right Italy
  fromChunks [Key "ugc_country_japan"      ] = Right Japan
  fromChunks [Key "ugc_country_netherlands"] = Right Netherlands
  fromChunks [Key "ugc_country_newzealand" ] = Right NewZealand
  fromChunks [Key "ugc_country_norway"     ] = Right Norway
  fromChunks [Key "ugc_country_singapore"  ] = Right Singapore
  fromChunks [Key "ugc_country_spain"      ] = Right Spain
  fromChunks [Key "ugc_country_sweden"     ] = Right Sweden
  fromChunks [Key "ugc_country_uk"         ] = Right UnitedKingdom
  fromChunks [Key "ugc_country_us"         ] = Right UnitedStates
  fromChunks cs = Left $ "Couldn't read as Country: " ++ show cs

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
  toChunks x =
    [ tagged "tool_version"             $ toChunks $ toolVersion     x
    , tagged "project_version"          $ toChunks $ projectVersion  x
    , tagged "metadata"                 $ toChunks $ metadata        x
    , tagged "gamedata"                 $ toChunks $ gamedata        x
    , tagged "languages"                $ toChunks $ languages       x
    , tagged "destination_file"         $ toChunks $ destinationFile x
    , tagged "midi"                     $ toChunks $ midi            x
    , tagged "dry_vox"                  $ toChunks $ dryVox          x
    , tagged "album_art" [tagged "file" $ toChunks $ albumArtFile    x]
    , tagged "tracks"                   $ toChunks $ tracks          x
    ]

instance FromChunks Project where
  fromChunks cs = Project
    <$> (getTag "tool_version"     cs  >>= fromChunks)
    <*> (getTag "project_version"  cs  >>= fromChunks)
    <*> (getTag "metadata"         cs  >>= fromChunks)
    <*> (getTag "gamedata"         cs  >>= fromChunks)
    <*> (getTag "languages"        cs  >>= fromChunks)
    <*> (getTag "destination_file" cs  >>= fromChunks)
    <*> (getTag "midi"             cs  >>= fromChunks)
    <*> (getTag "dry_vox"          cs  >>= fromChunks)
    <*> (getTag "album_art"        cs  >>= \cs'
      -> getTag "file"             cs' >>= fromChunks)
    <*> (getTag "tracks"           cs  >>= fromChunks)

instance DTAFormat Project where
  serialize p = DTA 0 $ Tree 0 [tagged "project" $ toChunks p]
  unserialize (DTA _ (Tree _ cs)) = getTag "project" cs >>= fromChunks

data Midi = Midi
  { midiFile     :: B8.ByteString
  , autogenTheme :: B8.ByteString
  -- ^  \"\" (Default), \"AggressiveMetal.rbtheme\", \"ArenaRock.rbtheme\",
  -- \"DarkHeavyRock.rbtheme\", \"DustyVintage.rbtheme\", \"EdgyProgRock.rbtheme\",
  -- \"FeelGoodPopRock.rbtheme\", \"GaragePunkRock.rbtheme\",
  -- \"PsychJamRock.rbtheme\", \"SlowJam.rbtheme\", \"SynthPop.rbtheme\"
  } deriving (Eq, Ord, Show, Read)

instance ToChunks Midi where
  toChunks m =
    [ tagged "file"          $ toChunks $ midiFile     m
    , tagged "autogen_theme" $ toChunks $ autogenTheme m
    ]

instance FromChunks Midi where
  fromChunks cs = Midi
    <$> (getTag "file"          cs >>= fromChunks)
    <*> (getTag "autogen_theme" cs >>= fromChunks)

data DryVox = DryVox
  { part0             :: DryVoxPart
  , part1             :: DryVoxPart
  , part2             :: DryVoxPart
  , tuningOffsetCents :: Float
  } deriving (Eq, Ord, Show, Read)

instance ToChunks DryVox where
  toChunks x =
    [ tagged "part0"               $ toChunks $ part0             x
    , tagged "part1"               $ toChunks $ part1             x
    , tagged "part2"               $ toChunks $ part2             x
    , tagged "tuning_offset_cents" $ toChunks $ tuningOffsetCents x
    ]

instance FromChunks DryVox where
  fromChunks cs = DryVox
    <$> (getTag "part0"               cs >>= fromChunks)
    <*> (getTag "part1"               cs >>= fromChunks)
    <*> (getTag "part2"               cs >>= fromChunks)
    <*> (getTag "tuning_offset_cents" cs >>= fromChunks)

data DryVoxPart = DryVoxPart
  { dryVoxFile    :: B8.ByteString
  , dryVoxEnabled :: Bool
  } deriving (Eq, Ord, Show, Read)

instance ToChunks DryVoxPart where
  toChunks x =
    [ tagged "file"    $ toChunks $ dryVoxFile    x
    , tagged "enabled" $ toChunks $ dryVoxEnabled x
    ]

instance FromChunks DryVoxPart where
  fromChunks cs = DryVoxPart
    <$> (getTag "file"    cs >>= fromChunks)
    <*> (getTag "enabled" cs >>= fromChunks)

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
  toChunks x =
    [ tagged "drum_layout" $ toChunks $ drumLayout x
    , tagged "drum_kit"    $ toChunks $ drumKit    x
    , tagged "drum_kick"   $ toChunks $ drumKick   x
    , tagged "drum_snare"  $ toChunks $ drumSnare  x
    , tagged "bass"        $ toChunks $ bass       x
    , tagged "guitar"      $ toChunks $ guitar     x
    , tagged "vocals"      $ toChunks $ vocals     x
    , tagged "keys"        $ toChunks $ keys       x
    , tagged "backing"     $ toChunks $ backing    x
    ]

instance FromChunks Tracks where
  fromChunks cs = Tracks
    <$> (getTag "drum_layout" cs >>= fromChunks)
    <*> (getTag "drum_kit"    cs >>= fromChunks)
    <*> (getTag "drum_kick"   cs >>= fromChunks)
    <*> (getTag "drum_snare"  cs >>= fromChunks)
    <*> (getTag "bass"        cs >>= fromChunks)
    <*> (getTag "guitar"      cs >>= fromChunks)
    <*> (getTag "vocals"      cs >>= fromChunks)
    <*> (getTag "keys"        cs >>= fromChunks)
    <*> (getTag "backing"     cs >>= fromChunks)


data DrumLayout = DrumLayoutKit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ToChunks DrumLayout where
  toChunks DrumLayoutKit = [Key "drum_layout_kit"]

instance FromChunks DrumLayout where
  fromChunks [Key "drum_layout_kit"] = Right DrumLayoutKit
  fromChunks cs = Left $ "Couldn't read as DryVoxPart: " ++ show cs

data AudioFile = AudioFile
  { audioEnabled :: Bool
  , channels     :: Integer
  , pan          :: Maybe (Float, Float)
  , vol          :: Maybe (Float, Float)
  , audioFile    :: B8.ByteString
  } deriving (Eq, Ord, Show, Read)

instance ToChunks AudioFile where
  toChunks x =
    [ tagged "enabled"  $ toChunks $ audioEnabled x
    , tagged "channels" $ toChunks $ channels     x
    , tagged "pan"      $ toChunks $ pan          x
    , tagged "vol"      $ toChunks $ vol          x
    , tagged "file"     $ toChunks $ audioFile    x
    ]

instance FromChunks AudioFile where
  fromChunks cs = AudioFile
    <$> (getTag "enabled"  cs >>= fromChunks)
    <*> (getTag "channels" cs >>= fromChunks)
    <*> (getTag "pan"      cs >>= fromChunks)
    <*> (getTag "vol"      cs >>= fromChunks)
    <*> (getTag "file"     cs >>= fromChunks)
