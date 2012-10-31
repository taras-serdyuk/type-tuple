{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Type.Tuple.Test.Test where
-- TODO: separate data generation, testing DSL and Interpreter API

import Control.Monad
import Language.Haskell.Interpreter
import Test.QuickCheck.Gen
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter


is, no :: String -> Interpreter ()

is = valid . feedInst interpInst
no = invalid . feedInst interpInst

-- TODO: refactor
feedInst :: (Class -> [Type] -> Type -> a) -> String -> a
feedInst f inst = f cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)


-- TODO: convert to class For
for1 :: (RenderType a, RenderType r, DataGenerator g a) => (Int, Class, a -> r) -> g -> Interpreter ()
for1 (n, cl, et) g = mapM_ (\x -> valid $ interpInst cl [renderType x] (renderType $ et x)) =<< typesGen l n gen
    where (gen, l) = generator g

for2 :: (DataGenerator g1 a, DataGenerator g2 b, RenderType a, RenderType b, RenderType r) => (Int, Class, a -> b -> r) -> (g1, g2) -> Interpreter ()
for2 (n, cl, et) (g1, g2) = valids2 cl et (typesGen xl n xGen) (typesGen yl n yGen)
    where ((xGen, xl), (yGen, yl)) = (generator g1, generator g2)

typesGen :: (MonadIO m) => Int -> Int -> Gen a -> m [a]
typesGen l n = liftIO . applyGen l . vectorOf n

eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)


-- TODO: delete
valids2 :: (RenderType a, RenderType b, RenderType r) => String -> (a -> b -> r) -> Interpreter [a] -> Interpreter [b] -> Interpreter ()
valids2 cl et xsm ysm = xsm >>= \xs -> ysm >>= zipWithM_ (\x y -> valid $ interpInst cl [renderType x, renderType y] (renderType $ et x y)) xs
