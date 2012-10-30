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


is, no :: (Functor m, MonadInterpreter m) => String -> m ()

is = valid . feedInst interpInst
no = invalid . feedInst interpInst

-- TODO: refactor
feedInst :: (Class -> [Type] -> Type -> a) -> String -> a
feedInst f inst = f cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)


-- TODO: convert to class For
for1 :: (RenderType r, Functor m, MonadInterpreter m) => (Int, Class, Type -> r) -> GenT -> m ()
for1 (n, cl, et) (gen, l) = mapM_ (\x -> valid $ interpInst cl [renderType x] (renderType $ et x)) =<< typesGen l n gen

for2 :: (RenderType r, Functor m, MonadInterpreter m) => (Int, Class, Type -> Type -> r) -> (GenT, GenT) -> m ()
for2 (n, cl, et) ((xGen, xl), (yGen, yl)) = valids2 cl et (typesGen xl n xGen) (typesGen yl n yGen)

typesGen :: (MonadIO m) => Int -> Int -> Gen a -> m [a]
typesGen l n = liftIO . applyGen l . vectorOf n

eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)


-- TODO: delete
valids2 :: (RenderType r, Functor m, MonadInterpreter m) => String -> (Type -> Type -> r) -> m [String] -> m [String] -> m ()
valids2 cl et xsm ysm = xsm >>= \xs -> ysm >>= zipWithM_ (\x y -> valid $ interpInst cl [renderType x, renderType y] (renderType $ et x y)) xs
