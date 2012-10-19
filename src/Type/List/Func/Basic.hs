--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.List.Func.Basic (
    Head, head,
    Tail, tail,
    Append, append
    ) where

import Type.List.Base


class (List a) => Head a b | a -> b where
    head :: a -> b
    head = nop

instance (List b) => Head (Cons a b) a


class (List a, List b) => Tail a b | a -> b where
    tail :: a -> b
    tail = nop

instance (List b) => Tail (Cons a b) b


class (List a, List b, List c) => Append a b c | a b -> c where
    append :: a -> b -> c
    append = nop

instance (List a) => Append Nil a a
--instance (List a) => Append a Nil a
instance (List a, List b, List c, Append a b c) => Append (Cons x a) b (Cons x c)

