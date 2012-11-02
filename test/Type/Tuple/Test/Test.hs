{-# LANGUAGE FlexibleInstances #-}

module Type.Tuple.Test.Test where

import Control.Monad
import Language.Haskell.Interpreter -- TODO: delete
import Test.QuickCheck.Gen
import Type.Tuple.Test.Data
import Type.Tuple.Test.Expression
import Type.Tuple.Test.Runner


class Same f where
    same :: Class -> f

instance (DataGenerator g a, RenderType a, RenderType r) => Same ((a -> r) -> g -> Gen (Interpreter ())) where
    same cl et gen = liftM (\x -> validInst $ Inst1 cl (renderType x) (renderType $ et x)) (generator gen)

instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) => Same ((a -> b -> r) -> g -> j -> Gen (Interpreter ())) where
    same cl et xGen yGen = liftM2 (\x y -> validInst $ Inst2 cl (renderType x) (renderType y) (renderType $ et x y)) (generator xGen) (generator yGen)


run :: Int -> Gen (Interpreter ()) -> TypeCheck ()
run n g = TypeCheck $ applyGen n g >>= sequence_

is, no :: String -> TypeCheck ()
is = TypeCheck . validInst . parseInst
no = TypeCheck . invalidInst . parseInst


validInst, invalidInst :: Instance -> Interpreter ()
validInst x = valid (renderType x) (applyInst x)
invalidInst x = invalid ("Invalid: " ++ renderType x) (applyInst x)
