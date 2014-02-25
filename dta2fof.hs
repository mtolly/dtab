{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as B8
import Data.Char (toUpper)
import qualified Data.Map as Map
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Data.DTA
import Data.DTA.Serialize
import qualified Data.DTA.Serialize.RB3 as R

songsDtaToFoF :: R.SongPackage -> [(String, String)]
songsDtaToFoF sp =
  [ ("name", B8.unpack $ R.name sp)
  , ("artist", B8.unpack $ R.artist sp)
  , ("genre", capitalize $ B8.unpack $ fromKeyword $ R.genre sp)
  , ("year", show $ R.yearReleased sp)
  , ("album", B8.unpack $ R.albumName sp)
  , ("version", show $ R.version sp)
  ] where capitalize = unwords . map capWord . words
          capWord (c : cs) = toUpper c : cs
          capWord "" = ""

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [fin, fout] -> fromDTA fin >>= \rb3 -> case unserialize rb3 of
    Left e -> error e
    Right dict -> case Map.toList $ fromDict dict of
      [(_, sp)] -> do
        let ini = unlines $ "[song]" : map makeLine (songsDtaToFoF sp)
            makeLine (x, y) = x ++ " = " ++ y
        _ <- evaluate $ length ini
        writeFile fout ini
      _ -> error "Not exactly 1 song in DTA file"
  _ -> getProgName >>= \prog -> do
    hPutStrLn stderr $ "Usage: "++prog++" songs.dta song.ini"
