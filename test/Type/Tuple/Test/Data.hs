{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Tuple.Test.Data where

import Control.Monad
import Data.List
import Test.QuickCheck.Gen
import Type.Tuple.Test.Text
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


maxSize :: Int
maxSize = 20

typeGen :: Gen Char
typeGen = elements types

applyGen :: (DataGenerator a b) => Int -> a -> IO [b]
applyGen n x = apply . vectorOf n $ generator x where
    apply gen = liftM (flip (unGen gen) maxSize) newStdGen

-- TODO: refactor
applyGen1 :: Int -> Gen a -> IO [a]
applyGen1 n x = apply $ vectorOf n x where
    apply gen = liftM (flip (unGen gen) maxSize) newStdGen
