{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.DTA.Serialize where

import Data.DTA.Base
import qualified Data.ByteString.Char8 as B8
import Control.Applicative (liftA2)
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Traversable as T

unserialize :: (FromChunks a) => DTA -> Either String a
unserialize (DTA _ (Tree _ cs)) = fromChunks cs

serialize :: (ToChunks a) => a -> DTA
serialize = DTA 0 . Tree 0 . toChunks

class ToChunks a where
  toChunks :: a -> [Chunk]

class FromChunks a where
  fromChunks :: [Chunk] -> Either String a

instance ToChunks DTA where
  toChunks = treeChunks . topTree

instance FromChunks DTA where
  fromChunks = Right . DTA 0 . Tree 0

instance ToChunks Chunk where
  toChunks x = [x]

instance FromChunks Chunk where
  fromChunks [x] = Right x
  fromChunks cs = Left $ "Expected 1 chunk, got: " ++ show cs

newtype Dict a = Dict { fromDict :: Map.Map B8.ByteString a }
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable, T.Traversable)

instance (ToChunks a) => ToChunks (Dict a) where
  toChunks = makeDict . fmap toChunks

instance (FromChunks a) => FromChunks (Dict a) where
  fromChunks cs = getDict cs >>= T.mapM fromChunks

getDict :: [Chunk] -> Either String (Dict [Chunk])
getDict cs = let
  toPair c = case c of
    Parens (Tree _ (Key k : rest)) -> Right (k, rest)
    _ -> Left $ "Expected (tag rest...), got: " ++ show c
  in fmap (Dict . Map.fromList) $ mapM toPair cs

makeDict :: Dict [Chunk] -> [Chunk]
makeDict (Dict m) =
  [ Parens $ Tree 0 $ Key k : v | (k, v) <- Map.toList m ]

dictLookup :: B8.ByteString -> Dict v -> Either String v
dictLookup k (Dict m) = case Map.lookup k m of
  Nothing -> Left $ "Couldn't find key " ++ show k
  Just v  -> Right v

makeDict' :: [(B8.ByteString, [Chunk])] -> [Chunk]
makeDict' = makeDict . Dict . Map.fromList

newtype InParens a = InParens { fromInParens :: a }
  deriving (Eq, Ord, Show, Read)

instance (ToChunks a) => ToChunks (InParens a) where
  toChunks (InParens xs) = [Parens $ Tree 0 $ toChunks xs]

instance (FromChunks a) => FromChunks (InParens a) where
  fromChunks [Parens (Tree _ cs)] = fmap InParens $ fromChunks cs
  fromChunks cs = Left $ "Couldn't read as InParens: " ++ show cs

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

instance (ToChunks a) => ToChunks [a] where
  toChunks = concatMap toChunks

instance (FromChunks a) => FromChunks [a] where
  fromChunks = mapM $ \x -> fromChunks [x]

newtype Keyword = Keyword { fromKeyword :: B8.ByteString }
  deriving (Eq, Ord, Show, Read)

instance ToChunks Keyword where
  toChunks (Keyword k) = [Key k]

instance FromChunks Keyword where
  fromChunks [Key k] = Right $ Keyword k
  fromChunks cs = Left $ "Couldn't read as Keyword: " ++ show cs

instance (ToChunks a, ToChunks b) => ToChunks (Either a b) where
  toChunks = either toChunks toChunks

instance (FromChunks a, FromChunks b) => FromChunks (Either a b) where
  fromChunks cs = case (fromChunks cs, fromChunks cs) of
    (Right l, _      ) -> Right $ Left l
    (_      , Right r) -> Right $ Right r
    (Left  _, Left  _) -> Left $ "Couldn't read as Either: " ++ show cs
