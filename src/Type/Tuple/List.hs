{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Tuple.List where

import Type.Tuple.Nat


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
type List10 a b c d e f g h i j = Cons a (List9 b c d e f g h i j)
type List11 a b c d e f g h i j k = Cons a (List10 b c d e f g h i j k)
type List12 a b c d e f g h i j k l = Cons a (List11 b c d e f g h i j k l)
type List13 a b c d e f g h i j k l m = Cons a (List12 b c d e f g h i j k l m)
type List14 a b c d e f g h i j k l m n = Cons a (List13 b c d e f g h i j k l m n)
type List15 a b c d e f g h i j k l m n o = Cons a (List14 b c d e f g h i j k l m n o)
type List16 a b c d e f g h i j k l m n o p = Cons a (List15 b c d e f g h i j k l m n o p)
type List17 a b c d e f g h i j k l m n o p q = Cons a (List16 b c d e f g h i j k l m n o p q)
type List18 a b c d e f g h i j k l m n o p q r = Cons a (List17 b c d e f g h i j k l m n o p q r)
type List19 a b c d e f g h i j k l m n o p q r s = Cons a (List18 b c d e f g h i j k l m n o p q r s)
type List20 a b c d e f g h i j k l m n o p q r s t = Cons a (List19 b c d e f g h i j k l m n o p q r s t)


class List a
instance List Nil
instance (List b) => List (Cons a b)


class Head a b | a -> b
instance Head (Cons a b) a

class Tail a b | a -> b
instance Tail (Cons a b) b

class Last a b | a -> b
instance Last (Cons a Nil) a
instance (Last b c) => Last (Cons a b) c

class Init a b | a -> b
instance Init (Cons a Nil) Nil
instance (Init (Cons b c) d) => Init (Cons a (Cons b c)) (Cons a d)

class Append a b c | a b -> c
instance Append Nil a a
instance (Append b c d) => Append (Cons a b) c (Cons a d)

class Length a b | a -> b
instance Length Nil Zero
instance (Length b c) => Length (Cons a b) (Succ c)


class Drop a b c | a b -> c
instance Drop Zero Nil Nil
instance Drop Zero a a
instance Drop a Nil Nil
instance (Drop a c d) => Drop (Succ a) (Cons b c) d
