-- | dtab application with two functions: binary <-> text, and decrypt/encrypt.
module Main where

import Data.DTA.Binary
import Data.DTA.PrettyPrint
import Data.DTA.Parse
import Data.DTA.Crypt
import System.Environment
import System.IO

main = getArgs >>= \args ->
  case args of
    (mode : fin : fout : rest) ->
      withHandleIn fin $ \hin ->
        withHandleOut fout $ \hout -> case mode of
          "-ba" -> hReadDTB hin >>= hWriteDTA hout
          "-ab" -> hReadDTA hin >>= hWriteDTB hout
          "-nd" -> hDecrypt newCrypt hin hout
          "-ne" -> hEncrypt newCrypt key hin hout
          "-od" -> hDecrypt oldCrypt hin hout
          "-oe" -> hEncrypt newCrypt key hin hout
          _ -> printUsage
          where key = case rest of
                  (str:_) -> read str
                  _       -> 0x30171609
    _ -> printUsage

withHandleIn :: String -> (Handle -> IO a) -> IO a
withHandleIn "-" f = f stdin
withHandleIn fp  f = withFile fp ReadMode f

withHandleOut :: String -> (Handle -> IO a) -> IO a
withHandleOut "-" f = f stdout
withHandleOut fp  f = withFile fp WriteMode f

printUsage :: IO ()
printUsage = mapM_ putStrLn
  [ "dtab v0.6, by onyxite. Built on earlier work by xorloser and deimos."
  , "Usage: dtab mode file-in file-out [encrypt-key]"
  , "Modes: -ba converts DTB (binary) to DTA (text)"
  , "       -ab converts DTA (text) to DTB (binary)"
  , "       -nd decrypts new-style DTB"
  , "       -ne encrypts new-style DTB, with optional key"
  , "       -od decrypts old-style DTB"
  , "       -oe encrypts old-style DTB, with optional key"
  , "Old-style is used in early PS2 games, new-style otherwise."
  , "Use a hyphen (-) for stdin (file-in) or stdout (file-out)." ]
