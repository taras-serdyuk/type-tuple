{-# LANGUAGE FlexibleInstances #-}

module Type.Tuple.Test.Test where

import Control.Monad
import Language.Haskell.Interpreter -- TODO: delete
import Test.QuickCheck.Gen
import Type.Tuple.Test.Data
import Type.Tuple.Test.Expression
import Type.Tuple.Test.Runner


type InterpGen = Gen (Interpreter ())


class Same f where
    same :: Class -> f

instance (DataGenerator g a, RenderType a, RenderType r) =>
    Same ((a -> r) -> g -> InterpGen) where
    
    same cl et gen = liftM (validInst . inst) (generator gen)
        where inst x = Inst1 cl (renderType x) (renderType (et x))

instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) =>
    Same ((a -> b -> r) -> g -> j -> InterpGen) where
    
    same cl et xGen yGen = liftM2 (\x -> validInst . inst x) (generator xGen) (generator yGen)
        where inst x y = Inst2 cl (renderType x) (renderType y) (renderType (et x y))


test :: Int -> InterpGen -> TypeCheck ()
test n g = TypeCheck $ applyGen n g >>= sequence_

is, no :: String -> TypeCheck ()
is = TypeCheck . validInst . parseInst
no = TypeCheck . invalidInst . parseInst


validInst, invalidInst :: Instance -> Interpreter ()
validInst x = valid (renderType x) (applyInst x)
invalidInst x = invalid ("Invalid: " ++ renderType x) (applyInst x)
