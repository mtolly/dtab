-- | QuickCheck tests for the dtab library.
module Data.DTA.Test where

import Data.DTA
import Data.Binary
import Data.DTA.Parse
import Data.DTA.PrettyPrint

prop_binaryRoundTrip :: DTA -> Bool
prop_binaryRoundTrip d = d == decode (encode d)

prop_textRoundTrip :: DTA -> Bool
prop_textRoundTrip d =
  topTree (renumberFrom 0 d)
  == topTree (renumberFrom 0 $ fromString $ toString d)
