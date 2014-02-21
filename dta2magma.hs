{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.DTA
import Data.DTA.Serialize
import Data.DTA.Serialize.Magma
import qualified Data.DTA.Serialize.RB3 as R
import Data.ByteString.Char8 ()
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import Control.Exception (evaluate)

data DrumMix
  = DrumMix0
  | DrumMix1
  | DrumMix2
  | DrumMix3
  | DrumMix4
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

songsDtaToMagma :: DrumMix -> R.SongPackage -> RBProj
songsDtaToMagma dm sp = RBProj $ Project
  { toolVersion = "110411_A"
  , projectVersion = R.version sp
  , metadata = Metadata
    { songName     = R.name sp
    , artistName   = R.artist sp
    , genre        = R.genre sp
    , subGenre     = R.subGenre sp
    , yearReleased = R.yearReleased sp
    , albumName    = R.albumName sp
    , author       = "Unknown Author" -- TODO
    , releaseLabel = "Unknown Label" -- TODO
    , country      = Keyword "ugc_country_us" -- TODO
    , price        = 160
    , trackNumber  = R.albumTrackNumber sp
    , hasAlbum     = True -- TODO
    }
  , gamedata = Gamedata
    { previewStartMs   = fst $ R.preview sp
    , rankGuitar       = 1 -- TODO
    , rankBass         = 1 -- TODO
    , rankDrum         = 1 -- TODO
    , rankVocals       = 1 -- TODO
    , rankKeys         = 1 -- TODO
    , rankProKeys      = 1 -- TODO
    , rankBand         = 1 -- TODO
    , vocalScrollSpeed = R.songScrollSpeed sp
    , animTempo        = R.animTempo sp
    , vocalGender      = R.vocalGender sp
    , vocalPercussion  = Tambourine -- TODO
    , vocalParts       = R.vocalParts $ R.song sp
    , guidePitchVolume = R.guidePitchVolume sp
    }
  , languages = Languages -- TODO
    { english  = True
    , french   = False
    , italian  = False
    , spanish  = False
    , german   = False
    , japanese = False
    }
  , destinationFile = "magma.rba"
  , midi            = Midi
    { midiFile     = "notes.mid"
    , autogenTheme = Left DefaultTheme
    }
  , dryVox = DryVox
    { part0 = DryVoxPart
      { dryVoxFile    = "dryvox1.wav"
      , dryVoxEnabled = R.vocalParts (R.song sp) >= 1
      }
    , part1 = DryVoxPart
      { dryVoxFile    = "dryvox2.wav"
      , dryVoxEnabled = R.vocalParts (R.song sp) >= 2
      }
    , part2 = DryVoxPart
      { dryVoxFile    = "dryvox3.wav"
      , dryVoxEnabled = R.vocalParts (R.song sp) >= 3
      }
    , tuningOffsetCents = R.tuningOffsetCents sp
    }
  , albumArt = AlbumArt
    { albumArtFile = "cover.bmp"
    }
  , tracks = Tracks
    { drumLayout = case dm of
      DrumMix0 -> Kit
      DrumMix1 -> KitKickSnare
      DrumMix2 -> KitKickSnare
      DrumMix3 -> KitKickSnare
      DrumMix4 -> KitKick
    , drumKit = AudioFile
      { audioEnabled = maybe False (/= 0) $ Map.lookup "drums" rankDict
      , channels     = fromIntegral $ length kitIndexes
      , pan          = pans kitIndexes
      , vol          = vols kitIndexes
      , audioFile    = "drums.wav"
      }
    , drumKick = AudioFile
      { audioEnabled = ((dm /= DrumMix0) &&) $
        maybe False (/= 0) $ Map.lookup "drums" rankDict
      , channels     = fromIntegral $ length kickIndexes
      , pan          = pans kickIndexes
      , vol          = vols kickIndexes
      , audioFile    = "kick.wav"
      }
    , drumSnare = AudioFile
      { audioEnabled = ((dm `notElem` [DrumMix0, DrumMix4]) &&) $
        maybe False (/= 0) $ Map.lookup "drums" rankDict
      , channels     = fromIntegral $ length snareIndexes
      , pan          = pans snareIndexes
      , vol          = vols snareIndexes
      , audioFile    = "snare.wav"
      }
    , bass = AudioFile
      { audioEnabled = maybe False (/= 0) $ Map.lookup "bass" rankDict
      , channels     = fromIntegral $ maybe 0 length $ Map.lookup "bass" trks
      , pan          = pans $ fromMaybe [] $ Map.lookup "bass" trks
      , vol          = vols $ fromMaybe [] $ Map.lookup "bass" trks
      , audioFile    = "bass.wav"
      }
    , guitar = AudioFile
      { audioEnabled = maybe False (/= 0) $ Map.lookup "guitar" rankDict
      , channels     = fromIntegral $ maybe 0 length $ Map.lookup "guitar" trks
      , pan          = pans $ fromMaybe [] $ Map.lookup "guitar" trks
      , vol          = vols $ fromMaybe [] $ Map.lookup "guitar" trks
      , audioFile    = "guitar.wav"
      }
    , vocals = AudioFile
      { audioEnabled = maybe False (/= 0) $ Map.lookup "vocals" rankDict
      , channels     = fromIntegral $ maybe 0 length $ Map.lookup "vocals" trks
      , pan          = pans $ fromMaybe [] $ Map.lookup "vocals" trks
      , vol          = vols $ fromMaybe [] $ Map.lookup "vocals" trks
      , audioFile    = "vocals.wav"
      }
    , keys = AudioFile
      { audioEnabled = maybe False (/= 0) $ Map.lookup "keys" rankDict
      , channels     = fromIntegral $ maybe 0 length $ Map.lookup "keys" trks
      , pan          = pans $ fromMaybe [] $ Map.lookup "keys" trks
      , vol          = vols $ fromMaybe [] $ Map.lookup "keys" trks
      , audioFile    = "keys.wav"
      }
    , backing = AudioFile
      { audioEnabled = True
      , channels     = fromIntegral $ length backingIndexes
      , pan          = pans backingIndexes
      , vol          = vols backingIndexes
      , audioFile    = "backing.wav"
      }
    }
  } where
    rankDict = fromDict $ R.rank sp
    trks = fromDict $ fmap fromInParens $ fromInParens $ R.tracks $ R.song sp
    pans, vols :: [Integer] -> [Float]
    pans = map $ \i -> fromInParens (R.pans $ R.song sp) !! fromIntegral i
    vols = map $ \i -> fromInParens (R.vols $ R.song sp) !! fromIntegral i
    drumIndexes = fromMaybe [] $ Map.lookup "drums" trks
    kitIndexes = reverse $ take 2 $ reverse drumIndexes
    kickIndexes = let
      n = case dm of
        DrumMix0 -> 0
        DrumMix1 -> 1
        DrumMix2 -> 1
        DrumMix3 -> 2
        DrumMix4 -> 1
      in take n drumIndexes
    snareIndexes = let
      n = case dm of
        DrumMix0 -> 0
        DrumMix1 -> 1
        DrumMix2 -> 2
        DrumMix3 -> 2
        DrumMix4 -> 0
      in take n $ drop (length kickIndexes) drumIndexes
    allIndexes = Set.fromList [0 .. sum (fromInParens $ R.tracksCount $ R.song sp) - 1]
    usedIndexes = Set.fromList $ concat $ Map.elems trks
    backingIndexes = Set.toAscList $ Set.difference allIndexes usedIndexes

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [mix, fin, fout] -> fromDTA fin >>= \rb3 -> case unserialize rb3 of
    Left e -> error e
    Right dict -> case Map.toList $ fromDict dict of
      [(_, sp)] -> do
        let dta = sToDTA $ serialize $
              songsDtaToMagma (read $ "DrumMix" ++ mix) sp
        _ <- evaluate $ length dta
        writeFile fout dta
      _ -> error "Not exactly 1 song in DTA file"
  _ -> getProgName >>= \prog -> do
    hPutStrLn stderr $ "Usage: "++prog++" drum-mix songs.dta magma.rbproj"
