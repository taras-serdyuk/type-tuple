{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.Test.Data where
-- TODO: legacy

import Control.Monad
import Data.List
import System.Random
import Test.QuickCheck.Gen
import Type.Tuple.Test.Text
import Type.Tuple.Test.Types


type Class = String
type Type = String


data TupleData = AnyTuple | NonEmptyTuple | HalfTuple
data NatData = Nat

-- TODO: embed length
-- TODO: other names
class DataGenerator a b | a -> b where
    generator :: a -> (Gen b, Int)

instance DataGenerator TupleData String where
    -- TODO: refactor
    generator AnyTuple = (listOf $ elements types, 20)
    generator NonEmptyTuple = (listOf1 $ elements types, 20)
    generator HalfTuple = (listOf $ elements types, 10)

instance DataGenerator NatData Int where
    -- TODO: refactor
    generator Nat = (elements [0 .. 20], 20)


-- TODO: other names
class RenderType a where
    renderType :: a -> Type

instance RenderType Char where
    renderType = return

instance RenderType String where
    renderType [] = "()"
    renderType [x] = parens ("Only" .- [x])
    renderType xs = parens (intersperse ',' xs)

instance RenderType Int where
    renderType x = "Nat" ++ show x


applyGen :: Int -> Gen a -> IO a
applyGen size gen = liftM (flip (unGen gen) size) newStdGen
