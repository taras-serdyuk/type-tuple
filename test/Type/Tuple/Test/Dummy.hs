{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.Test.Dummy where

import Type.Tuple.Base


class Tuple a


class (Tuple a, Tuple b, Tuple c) => Append a b c | a b -> c where
    append :: a -> b -> c
    append = nop


class (Tuple a, Tuple b) => Head a b | a -> b where
    head :: a -> b
    head = nop
