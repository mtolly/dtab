-- | dtab application with two functions: binary <-> text, and decrypt/encrypt.
module Main where

import System.Environment (getArgs)
import qualified System.IO as IO
import Data.Version (showVersion)

import Data.DTA
import Data.DTA.Crypt
import Paths_dtab (version)

main :: IO ()
main = getArgs >>= \args ->
  case args of
    (mode : fin : fout : rest) ->
      withHandleIn fin $ \hin ->
        withHandleOut fout $ \hout -> case mode of
          "-a" -> hFromDTB   hin >>= hToDTA hout
          "-A" -> hFromDTBv2 hin >>= hToDTA hout
          "-b" -> hFromDTA   hin >>= hToDTB hout . renumberFrom 1
          "-d" -> decryptHandle newCrypt hin hout
          "-e" -> encryptHandle newCrypt key hin hout
          "-D" -> decryptHandle oldCrypt hin hout
          "-E" -> encryptHandle oldCrypt key hin hout
          _ -> printUsage
          where key = case rest of
                  str : _ -> read str
                  _       -> 0x30171609
    _ -> printUsage

withHandleIn :: String -> (IO.Handle -> IO a) -> IO a
withHandleIn "-" f = f IO.stdin
withHandleIn fp  f = IO.withFile fp IO.ReadMode f

withHandleOut :: String -> (IO.Handle -> IO a) -> IO a
withHandleOut "-" f = f IO.stdout
withHandleOut fp  f = IO.withFile fp IO.WriteMode f

printUsage :: IO ()
printUsage = do
  let v = showVersion version
  mapM_ (IO.hPutStrLn IO.stderr)
    [ "dtab v"++v++", by onyxite. Built on earlier work by xorloser and deimos."
    , "Usage: dtab mode file-in file-out [encrypt-key]"
    , "Modes: -a converts DTB (binary) to DTA (text)"
    , "       -A converts Fantasia format DTB (binary) to DTA (text)"
    , "       -b converts DTA (text) to DTB (binary)"
    , "       -d decrypts new-style DTB"
    , "       -e encrypts new-style DTB, with optional key"
    , "       -D decrypts old-style DTB"
    , "       -E encrypts old-style DTB, with optional key"
    , "Old-style is used in early PS2 games, new-style otherwise."
    , "Use a hyphen (-) for stdin (file-in) or stdout (file-out)." ]
