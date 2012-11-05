{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.Test.Data where

import Control.Monad
import Control.Monad.Trans
import Test.QuickCheck.Gen
import Type.Tuple.Test.Types
import System.Random


data TupleData = AnyTuple | NonEmptyTuple | HalfTuple

data NatData = Nat


class DataGenerator a b | a -> b where
    generator :: a -> Gen b

instance DataGenerator TupleData String where
    generator AnyTuple = listOf typeGen
    generator NonEmptyTuple = listOf1 typeGen
    generator HalfTuple = resize size $ listOf typeGen
        where size = div maxSize 2

instance DataGenerator NatData Int where
    generator _ = elements [0 .. maxSize]


maxSize :: Int
maxSize = 20

typeGen :: Gen Char
typeGen = elements types

applyGen :: (MonadIO m) => Int -> Gen a -> m [a]
applyGen n x = liftIO . apply $ vectorOf n x where
    apply gen = liftM (flip (unGen gen) maxSize) newStdGen
