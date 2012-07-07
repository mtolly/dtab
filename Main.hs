-- | dtab application with two functions: binary <-> text, and decrypt/encrypt.
module Main where

import Data.DTA
import qualified Data.DTA.PrettyPrint as DTA
import qualified Data.DTA.Parse as DTA
import Data.DTA.Crypt
import System.Environment
import System.IO

main = getArgs >>= \args ->
  case args of
    (mode : fin : fout : rest) ->
      withHandleIn fin $ \hin ->
        withHandleOut fout $ \hout -> case mode of
          "-a" -> fromHandle hin >>= DTA.toHandle hout
          "-b" -> DTA.fromHandle hin >>= toHandle hout
          "-d" -> decryptHandle newCrypt hin hout
          "-e" -> encryptHandle newCrypt key hin hout
          "-D" -> decryptHandle oldCrypt hin hout
          "-E" -> encryptHandle newCrypt key hin hout
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
  [ "dtab v0.8, by onyxite. Built on earlier work by xorloser and deimos."
  , "Usage: dtab mode file-in file-out [encrypt-key]"
  , "Modes: -a converts DTB (binary) to DTA (text)"
  , "       -b converts DTA (text) to DTB (binary)"
  , "       -d decrypts new-style DTB"
  , "       -e encrypts new-style DTB, with optional key"
  , "       -D decrypts old-style DTB"
  , "       -E encrypts old-style DTB, with optional key"
  , "Old-style is used in early PS2 games, new-style otherwise."
  , "Use a hyphen (-) for stdin (file-in) or stdout (file-out)." ]
