{-# LANGUAGE OverloadedStrings #-}
module Data.DTA.Serialize.RB3 where

import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>))

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import Data.DTA
import Data.DTA.Serialize
import Data.DTA.Serialize.Magma (Gender(..))



type Path = Either B8.ByteString Keyword

data SongPackage = SongPackage { name :: B8.ByteString, artist :: B8.ByteString, master :: Bool, songId :: Either Integer Keyword, song :: Song, bank :: Maybe (Path), drumBank :: Maybe (Path), animTempo :: Either AnimTempo Integer, bandFailCue :: Maybe (Path), songScrollSpeed :: Integer, preview :: (Integer, Integer), songLength :: Integer, rank :: Dict Integer, solo :: Maybe (InParens [Keyword]), format :: Integer, version :: Integer, gameOrigin :: Keyword, rating :: Integer, genre :: Keyword, subGenre :: Maybe (Keyword), vocalGender :: Gender, shortVersion :: Maybe (Integer), yearReleased :: Integer, albumArt :: Maybe (Bool), albumName :: Maybe (B8.ByteString), albumTrackNumber :: Maybe (Integer), vocalTonicNote :: Maybe (Pitch), songTonality :: Maybe (Tonality), tuningOffsetCents :: Maybe (Float), realGuitarTuning :: Maybe (InParens [Integer]), realBassTuning :: Maybe (InParens [Integer]), guidePitchVolume :: Maybe (Float), encoding :: Maybe (Keyword) } deriving (Eq, Ord, Read, Show)

instance ToChunks SongPackage where { toChunks x = makeDict $ Dict $ Map.fromList $ [("name", toChunks $ name x)] ++ [("artist", toChunks $ artist x)] ++ [("master", toChunks $ master x)] ++ [("song_id", toChunks $ songId x)] ++ [("song", toChunks $ song x)] ++ (case bank x of { Nothing -> []; Just v -> [("bank", toChunks v)] }) ++ (case drumBank x of { Nothing -> []; Just v -> [("drum_bank", toChunks v)] }) ++ [("anim_tempo", toChunks $ animTempo x)] ++ (case bandFailCue x of { Nothing -> []; Just v -> [("band_fail_cue", toChunks v)] }) ++ [("song_scroll_speed", toChunks $ songScrollSpeed x)] ++ [("preview", toChunks $ preview x)] ++ [("song_length", toChunks $ songLength x)] ++ [("rank", toChunks $ rank x)] ++ (case solo x of { Nothing -> []; Just v -> [("solo", toChunks v)] }) ++ [("format", toChunks $ format x)] ++ [("version", toChunks $ version x)] ++ [("game_origin", toChunks $ gameOrigin x)] ++ [("rating", toChunks $ rating x)] ++ [("genre", toChunks $ genre x)] ++ (case subGenre x of { Nothing -> []; Just v -> [("sub_genre", toChunks v)] }) ++ [("vocal_gender", toChunks $ vocalGender x)] ++ (case shortVersion x of { Nothing -> []; Just v -> [("short_version", toChunks v)] }) ++ [("year_released", toChunks $ yearReleased x)] ++ (case albumArt x of { Nothing -> []; Just v -> [("album_art", toChunks v)] }) ++ (case albumName x of { Nothing -> []; Just v -> [("album_name", toChunks v)] }) ++ (case albumTrackNumber x of { Nothing -> []; Just v -> [("album_track_number", toChunks v)] }) ++ (case vocalTonicNote x of { Nothing -> []; Just v -> [("vocal_tonic_note", toChunks v)] }) ++ (case songTonality x of { Nothing -> []; Just v -> [("song_tonality", toChunks v)] }) ++ (case tuningOffsetCents x of { Nothing -> []; Just v -> [("tuning_offset_cents", toChunks v)] }) ++ (case realGuitarTuning x of { Nothing -> []; Just v -> [("real_guitar_tuning", toChunks v)] }) ++ (case realBassTuning x of { Nothing -> []; Just v -> [("real_bass_tuning", toChunks v)] }) ++ (case guidePitchVolume x of { Nothing -> []; Just v -> [("guide_pitch_volume", toChunks v)] }) ++ (case encoding x of { Nothing -> []; Just v -> [("encoding", toChunks v)] }) }

instance FromChunks SongPackage where { fromChunks = getDict >=> \d -> SongPackage <$> (dictLookup "name" d >>= fromChunks) <*> (dictLookup "artist" d >>= fromChunks) <*> (dictLookup "master" d >>= fromChunks) <*> (dictLookup "song_id" d >>= fromChunks) <*> (dictLookup "song" d >>= fromChunks) <*> (case dictLookup "bank" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "drum_bank" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (dictLookup "anim_tempo" d >>= fromChunks) <*> (case dictLookup "band_fail_cue" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (dictLookup "song_scroll_speed" d >>= fromChunks) <*> (dictLookup "preview" d >>= fromChunks) <*> (dictLookup "song_length" d >>= fromChunks) <*> (dictLookup "rank" d >>= fromChunks) <*> (case dictLookup "solo" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (dictLookup "format" d >>= fromChunks) <*> (dictLookup "version" d >>= fromChunks) <*> (dictLookup "game_origin" d >>= fromChunks) <*> (dictLookup "rating" d >>= fromChunks) <*> (dictLookup "genre" d >>= fromChunks) <*> (case dictLookup "sub_genre" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (dictLookup "vocal_gender" d >>= fromChunks) <*> (case dictLookup "short_version" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (dictLookup "year_released" d >>= fromChunks) <*> (case dictLookup "album_art" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "album_name" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "album_track_number" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "vocal_tonic_note" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "song_tonality" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "tuning_offset_cents" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "real_guitar_tuning" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "real_bass_tuning" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "guide_pitch_volume" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (case dictLookup "encoding" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) }

data Pitch = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks Pitch where { toChunks C = [Int 0]; toChunks CSharp = [Int 1]; toChunks D = [Int 2]; toChunks DSharp = [Int 3]; toChunks E = [Int 4]; toChunks F = [Int 5]; toChunks FSharp = [Int 6]; toChunks G = [Int 7]; toChunks GSharp = [Int 8]; toChunks A = [Int 9]; toChunks ASharp = [Int 10]; toChunks B = [Int 11] }

instance FromChunks Pitch where { fromChunks [Int 0] = Right C; fromChunks [Int 1] = Right CSharp; fromChunks [Int 2] = Right D; fromChunks [Int 3] = Right DSharp; fromChunks [Int 4] = Right E; fromChunks [Int 5] = Right F; fromChunks [Int 6] = Right FSharp; fromChunks [Int 7] = Right G; fromChunks [Int 8] = Right GSharp; fromChunks [Int 9] = Right A; fromChunks [Int 10] = Right ASharp; fromChunks [Int 11] = Right B; fromChunks cs = Left $ "Couldn't read as Pitch: " ++ show cs }

data Tonality = Major | Minor deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks Tonality where { toChunks Major = [Int 0]; toChunks Minor = [Int 1] }

instance FromChunks Tonality where { fromChunks [Int 0] = Right Major; fromChunks [Int 1] = Right Minor; fromChunks cs = Left $ "Couldn't read as Tonality: " ++ show cs }

data AnimTempo = KTempoSlow | KTempoMedium | KTempoFast deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance ToChunks AnimTempo where { toChunks KTempoSlow = [Key "kTempoSlow"]; toChunks KTempoMedium = [Key "kTempoMedium"]; toChunks KTempoFast = [Key "kTempoFast"] }

instance FromChunks AnimTempo where { fromChunks [Key "kTempoSlow"] = Right KTempoSlow; fromChunks [Key "kTempoMedium"] = Right KTempoMedium; fromChunks [Key "kTempoFast"] = Right KTempoFast; fromChunks cs = Left $ "Couldn't read as AnimTempo: " ++ show cs }

data Song = Song { songName :: B8.ByteString, tracksCount :: Maybe (InParens [Integer]), tracks :: InParens (Dict (Either Integer (InParens [Integer]))), vocalParts :: Integer, pans :: InParens [Float], vols :: InParens [Float], cores :: InParens [Integer], drumSolo :: DrumSounds, drumFreestyle :: DrumSounds } deriving (Eq, Ord, Read, Show)

instance ToChunks Song where { toChunks x = makeDict $ Dict $ Map.fromList $ [("name", toChunks $ songName x)] ++ (case tracksCount x of { Nothing -> []; Just v -> [("tracks_count", toChunks v)] }) ++ [("tracks", toChunks $ tracks x)] ++ [("vocal_parts", toChunks $ vocalParts x)] ++ [("pans", toChunks $ pans x)] ++ [("vols", toChunks $ vols x)] ++ [("cores", toChunks $ cores x)] ++ [("drum_solo", toChunks $ drumSolo x)] ++ [("drum_freestyle", toChunks $ drumFreestyle x)] }

instance FromChunks Song where { fromChunks = getDict >=> \d -> Song <$> (dictLookup "name" d >>= fromChunks) <*> (case dictLookup "tracks_count" d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v }) <*> (dictLookup "tracks" d >>= fromChunks) <*> (dictLookup "vocal_parts" d >>= fromChunks) <*> (dictLookup "pans" d >>= fromChunks) <*> (dictLookup "vols" d >>= fromChunks) <*> (dictLookup "cores" d >>= fromChunks) <*> (dictLookup "drum_solo" d >>= fromChunks) <*> (dictLookup "drum_freestyle" d >>= fromChunks) }

data DrumSounds = DrumSounds { seqs :: InParens [Keyword] } deriving (Eq, Ord, Read, Show)

instance ToChunks DrumSounds where { toChunks x = makeDict $ Dict $ Map.fromList $ [("seqs", toChunks $ seqs x)] }

instance FromChunks DrumSounds where { fromChunks = getDict >=> \d -> DrumSounds <$> (dictLookup "seqs" d >>= fromChunks) }
