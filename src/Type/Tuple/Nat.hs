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
type Nat11 = Succ Nat10
type Nat12 = Succ Nat11
type Nat13 = Succ Nat12
type Nat14 = Succ Nat13
type Nat15 = Succ Nat14
type Nat16 = Succ Nat15
type Nat17 = Succ Nat16
type Nat18 = Succ Nat17
type Nat19 = Succ Nat18
type Nat20 = Succ Nat19


class Nat a
instance Nat Zero
instance Nat (Succ a)
