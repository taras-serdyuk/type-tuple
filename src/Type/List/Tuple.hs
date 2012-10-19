{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.List.Tuple where

import Type.List.Base


data Tuple a

type List0 = Nil
type List1 a = Cons a List0
type List2 a b = Cons a (List1 b)
type List3 a b c = Cons a (List2 b c)
type List4 a b c d = Cons a (List3 b c d)
type List5 a b c d e = Cons a (List4 b c d e)
type List6 a b c d e f = Cons a (List5 b c d e f)
type List7 a b c d e f g = Cons a (List6 b c d e f g)
type List8 a b c d e f g h = Cons a (List7 b c d e f g h)
type List9 a b c d e f g h i = Cons a (List8 b c d e f g h i)


class (List a) => ListTuple a b | a -> b, b -> a where
    fromTuple :: a -> b
    toTuple :: b -> a
    
    fromTuple = nop
    toTuple = nop

instance ListTuple List0 ()
instance ListTuple (List1 a) (Tuple a)
instance ListTuple (List2 a b) (a, b)
instance ListTuple (List3 a b c) (a, b, c)
instance ListTuple (List4 a b c d) (a, b, c, d)
instance ListTuple (List5 a b c d e) (a, b, c, d, e)
instance ListTuple (List6 a b c d e f) (a, b, c, d, e, f)
instance ListTuple (List7 a b c d e f g) (a, b, c, d, e, f, g)
instance ListTuple (List8 a b c d e f g h) (a, b, c, d, e, f, g, h)
instance ListTuple (List9 a b c d e f g h i) (a, b, c, d, e, f, g, h, i)
