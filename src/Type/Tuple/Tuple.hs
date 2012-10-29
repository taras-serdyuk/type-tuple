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


class Head a b | a -> b
instance (TupleList a a', L.Head a' b) => Head a b

class Tail a b | a -> b
instance (TupleList a a', TupleList b b', L.Tail a' b') => Tail a b
