{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Tuple.Test.Data where

import Control.Applicative
import Control.Monad
import Data.List
import Test.QuickCheck.Gen
import Type.Tuple.Test.Text
import Type.Tuple.Test.Types
import System.Random


data TupleData = AnyTuple | NonEmptyTuple | HalfTuple
data NatData = Nat
data PairData a b = Pair a b

newtype Only a = Only { fromOnly :: a }


instance Monad Only where
    return = Only
    (Only x) >>= f = f x


class DataGenerator a b | a -> b where
    generator :: a -> Gen b

instance DataGenerator TupleData (Only String) where
    generator AnyTuple = onlyTypes listOf
    generator NonEmptyTuple = onlyTypes listOf1
    generator HalfTuple = resize size $ onlyTypes listOf
        where size = div maxSize 2

onlyTypes :: (Gen Char -> Gen String) -> Gen (Only String)
onlyTypes list = promote . Only . list $ typeGen

instance DataGenerator NatData (Only Int) where
    generator _ = promote . Only $ elements [0 .. maxSize]

instance (DataGenerator a (Only c), DataGenerator b (Only d)) => DataGenerator (PairData a b) (c, d) where
    generator (Pair x y) = (,) <$> fromOnlyGen x <*> fromOnlyGen y

fromOnlyGen :: (DataGenerator a (Only b)) => a -> Gen b
fromOnlyGen = fmap fromOnly . generator


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

instance (RenderType a) => RenderType (Only a) where
    renderType (Only a) = renderType a


maxSize :: Int
maxSize = 20

typeGen :: Gen Char
typeGen = elements types

applyGen :: (DataGenerator a b) => Int -> a -> IO [b]
applyGen n x = apply . vectorOf n $ generator x where
    apply gen = liftM (flip (unGen gen) maxSize) newStdGen
