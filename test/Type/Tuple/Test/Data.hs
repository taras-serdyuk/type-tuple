{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.Test.Data where

import Data.List
import Test.QuickCheck.Gen
import Type.Tuple.Test.Text
import Type.Tuple.Test.Types


data TupleData = AnyTuple | NonEmptyTuple | HalfTuple
data NatData = Nat


-- TODO: embed length
class DataGenerator a b | a -> b where
    generator :: a -> (Gen b, Int)

instance DataGenerator TupleData String where
    generator AnyTuple = (listOf typeGen, 20)
    generator NonEmptyTuple = (listOf1 typeGen, 20)
    generator HalfTuple = (listOf typeGen, 10)

typeGen :: Gen Char
typeGen = elements types

instance DataGenerator NatData Int where
    -- TODO: refactor
    generator Nat = (elements [0 .. 20], 20)


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
