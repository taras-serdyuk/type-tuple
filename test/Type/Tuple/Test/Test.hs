{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Type.Tuple.Test.Test where
-- TODO: testing DSL and Interpreter API

import Control.Monad
import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter


type Class = String


is, no :: String -> Interpreter ()

is = valid . feedInst interpInst
no = invalid . feedInst interpInst

-- TODO: refactor
feedInst :: (Class -> [String] -> String -> a) -> String -> a
feedInst f inst = f cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)


-- TODO: convert to class For
for1 :: (RenderType a, RenderType r, DataGenerator g a) => (Int, Class, a -> r) -> g -> Interpreter ()
for1 (n, cl, et) gen = mapM_ (\x -> valid $ interpInst cl [renderType x] (renderType $ et x)) =<< liftIO (applyGen n gen)

for2 :: (DataGenerator g1 a, DataGenerator g2 b, RenderType a, RenderType b, RenderType r) => (Int, Class, a -> b -> r) -> (g1, g2) -> Interpreter ()
for2 (n, cl, et) (xGen, yGen) = valids2 cl et (liftIO (applyGen n xGen)) (liftIO (applyGen n yGen))

eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)


-- TODO: delete
valids2 :: (RenderType a, RenderType b, RenderType r) => String -> (a -> b -> r) -> Interpreter [a] -> Interpreter [b] -> Interpreter ()
valids2 cl et xsm ysm = xsm >>= \xs -> ysm >>= zipWithM_ (\x y -> valid $ interpInst cl [renderType x, renderType y] (renderType $ et x y)) xs
