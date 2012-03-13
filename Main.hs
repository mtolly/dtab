-- | dtab application with two functions: binary <-> text, and decrypt/encrypt.
module Main where

import Data.DTA.Binary
import Data.DTA.PrettyPrint
import Data.DTA.Parse
import Data.DTA.Crypt
import System.Environment
import System.IO
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

decodeLatin1 :: B8.ByteString -> T.Text
decodeLatin1 = T.pack . B8.unpack

encodeLatin1 :: T.Text -> B8.ByteString
encodeLatin1 = B8.pack . T.unpack

main = getArgs >>= \args -> do
  case args of
    (mode : fin : fout : rest) ->
      withHandleIn fin $ \hin ->
        withHandleOut fout $ \hout -> case mode of
          "-a" -> hReadDTB decodeLatin1 hin >>= hWriteDTA encodeLatin1 hout
          "-b" -> hReadDTA decodeLatin1 hin >>= hWriteDTB encodeLatin1 hout
          "-A" -> hReadDTB decodeUtf8 hin >>= hWriteDTA encodeUtf8 hout
          "-B" -> hReadDTA decodeUtf8 hin >>= hWriteDTB encodeUtf8 hout
          "-d" -> hDecrypt newCrypt hin hout
          "-e" -> hEncrypt newCrypt key hin hout
          "-D" -> hDecrypt oldCrypt hin hout
          "-E" -> hEncrypt newCrypt key hin hout
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
  , "Modes: -a converts DTB (binary) to DTA (text), latin-1 encoding"
  , "       -b converts DTA (text) to DTB (binary), latin-1 encoding"
  , "       -A converts DTB (binary) to DTA (text), utf-8 encoding"
  , "       -B converts DTA (text) to DTB (binary), utf-8 encoding"
  , "       -d decrypts new-style DTB"
  , "       -e encrypts new-style DTB, with optional key"
  , "       -D decrypts old-style DTB"
  , "       -E encrypts old-style DTB, with optional key"
  , "Old-style is used in early PS2 games, new-style otherwise."
  , "Use a hyphen (-) for stdin (file-in) or stdout (file-out)." ]
