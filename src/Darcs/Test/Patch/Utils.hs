module Darcs.Test.Patch.Utils
    ( testConditional, testStringList )
    where

import Test.Framework ( Test )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit ( assertFailure )
import Test.QuickCheck ( Arbitrary, (==>) )

-- | Turns a condition and a test function into a conditional quickcheck
--   property that can be run by test-framework.
testConditional :: (Arbitrary p, Show p, Show q) => String         -- ^ Test name
                                                 -> (p -> Bool)    -- ^ Condition
                                                 -> (p -> Maybe q) -- ^ Test function
                                                 -> Test
testConditional name nottrivial t = testProperty name t'
    where t' p = nottrivial p ==> case t p of
                                  Nothing -> True
                                  Just q -> error $ show q

-- | Utility function to run old tests that return a list of error messages,
--   with the empty list meaning success.
testStringList :: String -> [String] -> Test
testStringList name test = testCase name $ mapM_ assertFailure test
