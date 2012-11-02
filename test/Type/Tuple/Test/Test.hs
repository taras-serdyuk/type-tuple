{-# LANGUAGE FlexibleInstances #-}

module Type.Tuple.Test.Test where

import Control.Applicative
import Control.Monad
import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type ForF f = Int -> Class -> f -> TypeCheck ()


class For f where
    for :: f

instance (DataGenerator g a, RenderType a, RenderType r) => For (g -> ForF (a -> r)) where
    for gen n cl et = TypeCheck $ mapM_ f =<< applyGen n gen
        where f x = validInst $ Inst1 cl (renderType x) (renderType $ et x)

instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) => For (g -> j -> ForF (a -> b -> r)) where
    for xgen ygen n cl et = TypeCheck . join $ zipWithM_ f <$> applyGen n xgen <*> applyGen n ygen
        where f x y = validInst $ Inst2 cl (renderType x) (renderType y) (renderType $ et x y)

-- TODO: refactor to return granular tests instead of batches
same :: Int -> Class -> f -> ForF f -> TypeCheck ()
same n cl et f = f n cl et

is, no :: String -> TypeCheck ()
is = TypeCheck . validInst . parseInst
no = TypeCheck . invalidInst . parseInst


validInst, invalidInst :: Instance -> Interpreter ()
validInst x = valid (renderType x) (applyInst x)
invalidInst x = invalid ("Invalid: " ++ renderType x) (applyInst x)
