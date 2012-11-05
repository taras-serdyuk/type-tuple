{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Tuple.Tuple where

import qualified Type.Tuple.List as L


data Only a


class TupleList a b | a -> b, b -> a
instance TupleList () L.List0
instance TupleList (Only a) (L.List1 a)
instance TupleList (a, b) (L.List2 a b)
instance TupleList (a, b, c) (L.List3 a b c)
instance TupleList (a, b, c, d) (L.List4 a b c d)
instance TupleList (a, b, c, d, e) (L.List5 a b c d e)
instance TupleList (a, b, c, d, e, f) (L.List6 a b c d e f)
instance TupleList (a, b, c, d, e, f, g) (L.List7 a b c d e f g)
instance TupleList (a, b, c, d, e, f, g, h) (L.List8 a b c d e f g h)
instance TupleList (a, b, c, d, e, f, g, h, i) (L.List9 a b c d e f g h i)
instance TupleList (a, b, c, d, e, f, g, h, i, j) (L.List10 a b c d e f g h i j)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k) (L.List11 a b c d e f g h i j k)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l) (L.List12 a b c d e f g h i j k l)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m) (L.List13 a b c d e f g h i j k l m)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (L.List14 a b c d e f g h i j k l m n)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (L.List15 a b c d e f g h i j k l m n o)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) (L.List16 a b c d e f g h i j k l m n o p)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) (L.List17 a b c d e f g h i j k l m n o p q)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) (L.List18 a b c d e f g h i j k l m n o p q r)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) (L.List19 a b c d e f g h i j k l m n o p q r s)
instance TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) (L.List20 a b c d e f g h i j k l m n o p q r s t)


class Head a b | a -> b
instance (TupleList a a', L.Head a' b) => Head a b

class Tail a b | a -> b
instance (TupleList a a', TupleList b b', L.Tail a' b') => Tail a b

class Last a b | a -> b
instance (TupleList a a', L.Last a' b) => Last a b

class Init a b | a -> b
instance (TupleList a a', TupleList b b', L.Init a' b') => Init a b

class Append a b c | a b -> c
instance (TupleList a a', TupleList b b', TupleList c c', L.Append a' b' c') => Append a b c

class Length a b | a -> b
instance (TupleList a a', L.Length a' b) => Length a b


class Drop a b c | a b -> b
instance (TupleList b b', TupleList c c', L.Drop a b' c') => Drop a b c


class Reverse a b | a -> b
instance (TupleList a a', TupleList b b', L.Reverse a' b') => Reverse a b


class Replicate a b c | a b -> c
instance (TupleList c c', L.Replicate a b c') => Replicate a b c
