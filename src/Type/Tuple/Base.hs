{-# LANGUAGE EmptyDataDecls #-}

module Type.Tuple.Base (
    Nil, Cons, List, nop) where


data Nil
data Cons a b


class List a

instance List Nil
instance (List b) => List (Cons a b)


nop :: a
nop = error "No Operation"
