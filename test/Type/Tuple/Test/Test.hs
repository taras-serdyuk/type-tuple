{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Type.Tuple.Test.Test where

import Language.Haskell.Interpreter
import Type.Tuple.Test.Interpreter


type Class = String
type Type = String


-- Convert to class
for1 :: (MonadInterpreter m) => (Int, Class, Type -> Type) -> Type -> m ()
for1 = undefined

is, no :: (Functor m, MonadInterpreter m) => String -> m ()

is = valid . feedInst interpInst
no = invalid . feedInst interpInst

feedInst :: (Class -> [Type] -> Type -> a) -> String -> a
feedInst f inst = f cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)


eq :: Int -> String -> a -> (Int, String, a)
eq = undefined


list' :: Type
list' = undefined

{--
is "Head (A, B) A"
no "Head () a"

eq 100 "Init" (tuple . init) `for` list'

eq 100 "Append" (\x y -> tuple (x ++ y)) `for` (list2, list2)

eq 10 "Length" (("Nat" ++) . show . length) `for` list
--}