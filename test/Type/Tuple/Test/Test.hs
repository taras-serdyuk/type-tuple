{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Type.Tuple.Test.Test where

import Control.Monad
import Language.Haskell.Interpreter
import Test.QuickCheck.Gen
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Text

-- TODO: separate data generation, testing DSL and Interpreter API


type Class = String
type Type = String
type GenT = (Int -> Gen [Type], Int)


is, no :: (Functor m, MonadInterpreter m) => String -> m ()

is = valid . feedInst interpInst
no = invalid . feedInst interpInst

feedInst :: (Class -> [Type] -> Type -> a) -> String -> a
feedInst f inst = f cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)


-- TODO: convert to class For
for1 :: (Functor m, MonadInterpreter m) => (Int, Class, Type -> Type) -> GenT -> m ()
for1 (n, cl, et) (gen, l) = valids cl et (liftIO . applyGen l $ gen n)

for2 :: (Functor m, MonadInterpreter m) => (Int, Class, Type -> Type -> Type) -> (GenT, GenT) -> m ()
for2 (n, cl, et) ((xGen, xl), (yGen, yl)) = valids2 cl et (liftIO . applyGen xl $ xGen n) (liftIO . applyGen yl $ yGen n)

eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)


list :: GenT
list = (inputsGen, 10)

list' :: GenT
list' = (inputsGen1, 10)

list2 :: GenT
list2 = (inputsGen, 5)


valids :: (Functor m, MonadInterpreter m) => String -> String2 -> m [String] -> m ()
valids cl et xsm = xsm >>= mapM_ (\x -> valid $ interpInst cl [tuple x] (et x))

valids2 :: (Functor m, MonadInterpreter m) => String -> String3 -> m [String] -> m [String] -> m ()
valids2 cl et xsm ysm = xsm >>= \xs -> ysm >>= zipWithM_ (\x y -> valid $ interpInst cl [tuple x, tuple y] (et x y)) xs
