{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Type.Tuple.List where


data Nil
data Cons a b

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


class List a
instance List Nil
instance (List b) => List (Cons a b)


class Head a b | a -> b
instance Head (Cons a b) a

class Tail a b | a -> b
instance Tail (Cons a b) b
