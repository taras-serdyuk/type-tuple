{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.Test.Data where

import Control.Monad
import Control.Monad.Trans
import Test.QuickCheck.Gen
import Type.Tuple.Test.Types
import System.Random


data ElementData = Elem

data NaturalData = Nat

data TupleData = AnyTuple | NonEmptyTuple | HalfTuple | MicroTuple


class DataGenerator a b | a -> b where
    generator :: a -> Gen b

instance DataGenerator ElementData Char where
    generator _ = typeGen

instance DataGenerator NaturalData Int where
    generator _ = elements [0 .. maxSize]

instance DataGenerator TupleData String where
    generator AnyTuple = typeListGen
    generator NonEmptyTuple = listOf1 typeGen
    generator MicroTuple = resize 1 typeListGen
    generator HalfTuple = resize (div maxSize 2) typeListGen


maxSize :: Int
maxSize = 20

typeGen :: Gen Char
typeGen = elements types

typeListGen :: Gen String
typeListGen = listOf typeGen

applyGen :: (MonadIO m) => Int -> Gen a -> m [a]
applyGen n x = liftIO . apply $ vectorOf n x where
    apply gen = liftM (flip (unGen gen) maxSize) newStdGen
