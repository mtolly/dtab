-- | QuickCheck tests for the dtab library.
module Main where

import Data.DTA
import Data.Binary
import Data.DTA.Parse
import Data.DTA.PrettyPrint
import Data.DTA.Crypt
import qualified Data.ByteString.Lazy as BL
import Test.QuickCheck

main = do
  runTest prop_binaryRoundTrip "binary round trip is equal"
  runTest prop_textRoundTrip "text round trip is equal (disregarding nodeID)"
  runTest prop_renumberEquiv "renumber doesn't change structure"
  runTest prop_renumberEquiv "renumber once = renumber twice"
  runTest prop_newCryptRoundTrip "new encryption round trip is equal"
  runTest prop_oldCryptRoundTrip "old encryption round trip is equal"

runTest :: (Testable a) => a -> String -> IO ()
runTest x str = putStrLn ("Testing: " ++ str) >> quickCheck x

newtype AnyLazy = AL { fromAL :: BL.ByteString }
  deriving (Eq, Ord, Show)
instance Arbitrary AnyLazy where
  arbitrary = fmap (AL . BL.pack) arbitrary

prop_binaryRoundTrip :: DTA -> Bool
prop_binaryRoundTrip d = d == decode (encode d)

prop_textRoundTrip :: DTA -> Bool
prop_textRoundTrip d = equivDTA d $ fromString $ toString d

prop_renumberEquiv :: DTA -> Bool
prop_renumberEquiv d = equivDTA d $ renumberFrom 0 d

prop_renumberTwice :: Word32 -> DTA -> Bool
prop_renumberTwice n d = let d' = renumberFrom n d in d' == renumberFrom n d'

equivDTA :: DTA -> DTA -> Bool
equivDTA (DTA _ tx) (DTA _ ty) = equivTree tx ty where
  equivTree (Tree _ cx) (Tree _ cy) = and $ zipWith equivChunk cx cy
  equivChunk (Parens x) (Parens y) = equivTree x y
  equivChunk (Braces x) (Braces y) = equivTree x y
  equivChunk (Brackets x) (Brackets y) = equivTree x y
  equivChunk x y = x == y

prop_newCryptRoundTrip, prop_oldCryptRoundTrip :: Key -> AnyLazy -> Bool
prop_newCryptRoundTrip k (AL b) = b == decrypt newCrypt (encrypt newCrypt k b)
prop_oldCryptRoundTrip k (AL b) = b == decrypt oldCrypt (encrypt oldCrypt k b)
