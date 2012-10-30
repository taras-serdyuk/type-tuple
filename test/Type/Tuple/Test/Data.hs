{-# LANGUAGE FlexibleInstances #-}

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
type GenT = (Gen Type, Int)

-- TODO: data generators
data Data = Tuple | NonEmptyTuple | HalfTuple | Nat


class RenderType a where
    renderType :: a -> Type

instance RenderType Char where
    renderType = return

instance RenderType String where
    renderType = tuple

instance RenderType Int where
    renderType x = "Nat" ++ show x


list :: GenT
list = (inputsGen, 20)

list' :: GenT
list' = (inputsGen1, 20)

list2 :: GenT
list2 = (inputsGen, 10)


-- TODO: delete
tuple :: String2
tuple [] = "()"
tuple [x] = parens ("Only" .- [x])
tuple xs = parens (intersperse ',' xs)


inputsGen :: Gen Type
inputsGen = listOf $ elements types

inputsGen1 :: Gen Type
inputsGen1 = listOf1 $ elements types

applyGen :: Int -> Gen a -> IO a
applyGen size gen = liftM (flip (unGen gen) size) newStdGen
