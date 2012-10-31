{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.Test.Data where

import Control.Monad
import Data.List
import Test.QuickCheck.Gen
import Type.Tuple.Test.Text
import Type.Tuple.Test.Types
import System.Random


data MaxTupleData = AnyTuple | NonEmptyTuple
data HalfTupleData = HalfTuple
data NatData = Nat


class DataGenerator a b | a -> b where
    generator :: a -> Gen b
    size :: a -> Int
    size _ = maxSize

maxSize :: Int
maxSize = 20


instance DataGenerator MaxTupleData String where
    generator AnyTuple = listOf typeGen
    generator NonEmptyTuple = listOf1 typeGen

instance DataGenerator HalfTupleData String where
    generator _ = listOf typeGen
    size _ = div maxSize 2

typeGen :: Gen Char
typeGen = elements types


instance DataGenerator NatData Int where
    generator _ = elements [0 .. maxSize]


class RenderType a where
    renderType :: a -> String

instance RenderType Char where
    renderType = return

instance RenderType String where
    renderType [] = "()"
    renderType [x] = parens ("Only" .- [x])
    renderType xs = parens (intersperse ',' xs)

instance RenderType Int where
    renderType x = "Nat" ++ show x


applyGen :: (DataGenerator a b) => Int -> a -> IO [b]
applyGen n x = apply $ vectorOf n (generator x) where
    apply gen = liftM (flip (unGen gen) (size x)) newStdGen
