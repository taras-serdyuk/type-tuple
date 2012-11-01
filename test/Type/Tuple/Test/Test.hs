{-# LANGUAGE ExistentialQuantification #-}
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
        where f x = valid' $ Inst1 cl (renderType x) (renderType $ et x)

instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) => For (g -> j -> ForF (a -> b -> r)) where
    for xgen ygen n cl et = join $ zipWithM_ f <$> applyGen n xgen <*> applyGen n ygen
        where f x y = valid' $ Inst2 cl (renderType x) (renderType y) (renderType $ et x y)

valid' :: Instance -> Interpreter ()
valid' inst = valid (renderType inst) expr where
    expr = applyClass (instClass inst) (instParams inst) (instResult inst)


is, no :: String -> Interpreter ()
is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)

same :: Int -> Class -> f -> ForF f -> Interpreter ()
same n cl et f = f n cl et
