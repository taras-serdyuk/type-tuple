
module Type.Tuple.Test.Data where
-- TODO: legacy

import Control.Monad
import Data.List
import System.Random
import Test.QuickCheck.Gen
import Type.Tuple.Test.Text
import Type.Tuple.Test.Types


tuple :: String2
tuple [] = "()"
tuple [x] = parens ("Only" .- [x])
tuple xs = parens (intersperse ',' xs)


inputsGen :: Int -> Gen [String]
inputsGen n = vectorOf n (listOf $ elements types)

inputsGen1 :: Int -> Gen [String]
inputsGen1 n = vectorOf n (listOf1 $ elements types)

applyGen :: Int -> Gen a -> IO a
applyGen size gen = liftM (flip (unGen gen) size) newStdGen
