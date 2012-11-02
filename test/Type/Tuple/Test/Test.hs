{-# LANGUAGE FlexibleInstances #-}

module Type.Tuple.Test.Test where

import Control.Monad
import Test.QuickCheck.Gen
import Type.Tuple.Test.Data
import Type.Tuple.Test.Expression
import Type.Tuple.Test.Instance
import Type.Tuple.Test.Runner


class Same f where
    same :: Class -> f

instance (DataGenerator g a, RenderType a, RenderType r) =>
    Same ((a -> r) -> g -> Gen Interp) where
    
    same cl et gen = liftM (validInst . inst) (generator gen)
        where inst x = Inst1 cl (renderType x) (renderType (et x))

instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) =>
    Same ((a -> b -> r) -> g -> j -> Gen Interp) where
    
    same cl et xGen yGen = liftM2 (\x -> validInst . inst x) (generator xGen) (generator yGen)
        where inst x y = Inst2 cl (renderType x) (renderType y) (renderType (et x y))


test :: Int -> Gen Interp -> Test ()
test n g = Test $ applyGen n g >>= sequence_

is, no :: String -> Test ()
is = Test . validInst . parseInst
no = Test . invalidInst . parseInst


validInst, invalidInst :: Instance -> Interp
validInst x = valid (renderType x) (applyInst x)
invalidInst x = invalid ("Invalid: " ++ renderType x) (applyInst x)
