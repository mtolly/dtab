module Data.DTA.Serialize where

import Data.DTA.Base
import qualified Data.ByteString.Char8 as B8
import Control.Applicative (liftA2)

class DTAFormat a where
  serialize   :: a -> DTA
  unserialize :: DTA -> Either String a

class ToChunks a where
  toChunks :: a -> [Chunk]

class FromChunks a where
  fromChunks :: [Chunk] -> Either String a

instance ToChunks DTA where
  toChunks = treeChunks . topTree

instance FromChunks DTA where
  fromChunks = Right . DTA 0 . Tree 0

getTag :: B8.ByteString -> [Chunk] -> Either String [Chunk]
getTag t cs =
  case [ xs | Parens (Tree _ (Key x : xs)) <- cs, x == t ] of
    [rest] -> Right rest
    xs     -> Left $ show (length xs) ++ " matches for tag " ++ B8.unpack t

tagged :: B8.ByteString -> [Chunk] -> Chunk
tagged t cs = Parens $ Tree 0 $ Key t : cs

instance ToChunks Bool where
  toChunks True = [Int 1]
  toChunks False = [Int 0]

instance FromChunks Bool where
  fromChunks [Int 1] = Right True
  fromChunks [Int 0] = Right False
  fromChunks cs = Left $ "Couldn't read as Bool: " ++ show cs

instance ToChunks Integer where
  toChunks i = [Int $ fromIntegral i]

instance FromChunks Integer where
  fromChunks [Int i] = Right $ fromIntegral i
  fromChunks cs = Left $ "Couldn't read as Integer: " ++ show cs

instance ToChunks Float where
  toChunks f = [Float f]

instance FromChunks Float where
  fromChunks [Float f] = Right f
  fromChunks cs = Left $ "Couldn't read as Float: " ++ show cs

instance ToChunks B8.ByteString where
  toChunks bs = [String bs]

instance FromChunks B8.ByteString where
  fromChunks [String bs] = Right bs
  fromChunks cs = Left $ "Couldn't read as ByteString: " ++ show cs

instance (ToChunks a, ToChunks b) => ToChunks (a, b) where
  toChunks (x, y) = toChunks x ++ toChunks y

instance (FromChunks a, FromChunks b) => FromChunks (a, b) where
  fromChunks [x, y] = liftA2 (,) (fromChunks [x]) (fromChunks [y])
  fromChunks cs = Left $ "Couldn't read as pair: " ++ show cs

instance (ToChunks a) => ToChunks (Maybe a) where
  toChunks Nothing = []
  toChunks (Just x) = toChunks x

instance (FromChunks a) => FromChunks (Maybe a) where
  fromChunks [] = Right Nothing
  fromChunks cs = fmap Just $ fromChunks cs
