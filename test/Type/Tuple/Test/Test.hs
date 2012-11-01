{-# LANGUAGE FlexibleInstances #-}

module Type.Tuple.Test.Test where

import Control.Applicative
import Control.Monad
import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type ForF f = Int -> Class -> f -> Interpreter ()


class For f where
    for :: f

instance (DataGenerator g a, RenderType a, RenderType r) => For (g -> ForF (a -> r)) where
    for gen n cl et = mapM_ f =<< applyGen n gen
        where f x = validInst $ Inst1 cl (renderType x) (renderType $ et x)

instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) => For (g -> j -> ForF (a -> b -> r)) where
    for xgen ygen n cl et = join $ zipWithM_ f <$> applyGen n xgen <*> applyGen n ygen
        where f x y = validInst $ Inst2 cl (renderType x) (renderType y) (renderType $ et x y)


same :: Int -> Class -> f -> ForF f -> Interpreter ()
same n cl et f = f n cl et

is, no :: String -> Interpreter ()
is = validInst . parseInst
no = invalidInst . parseInst


validInst, invalidInst :: Instance -> Interpreter ()
validInst x = valid (renderType x) (applyInst x)
invalidInst x = invalid ("Invalid: " ++ renderType x) (applyInst x)
