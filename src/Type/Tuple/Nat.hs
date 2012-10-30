{-# LANGUAGE EmptyDataDecls #-}

module Type.Tuple.Nat where


data Zero
data Succ a

type Nat0 = Zero
type Nat1 = Succ Nat0
type Nat2 = Succ Nat1
type Nat3 = Succ Nat2
type Nat4 = Succ Nat3
type Nat5 = Succ Nat4
type Nat6 = Succ Nat5
type Nat7 = Succ Nat6
type Nat8 = Succ Nat7
type Nat9 = Succ Nat8
type Nat10 = Succ Nat9


class Nat a
instance Nat Zero
instance Nat (Succ a)
