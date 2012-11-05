{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Instance where

import Data.List
import Type.Tuple.Test.Text


type Type = String
type Class = String

data Instance =
    Inst1 Class Type Type |
    Inst2 Class Type Type Type


class RenderType a where
    renderType :: a -> Type

instance RenderType Char where
    renderType = return

instance RenderType Int where
    renderType x = "Nat" ++ show x

instance RenderType String where
    renderType [] = "()"
    renderType [x] = parens ("Only" .- [x])
    renderType xs = parens (intersperse ',' xs)

instance RenderType Instance where
    renderType x = unwords (instClass x : instParams x ++ [instResult x])


instClass :: Instance -> Class
instClass (Inst1 cl _ _) = cl
instClass (Inst2 cl _ _ _) = cl

instParams :: Instance -> [Type]
instParams (Inst1 _ x _) = [x]
instParams (Inst2 _ x y _) = [x, y]

instResult :: Instance -> Type
instResult (Inst1 _ _ x) = x
instResult (Inst2 _ _ _ x) = x


mkInst :: Class -> [Type] -> Instance
mkInst cl [a, b] = Inst1 cl a b
mkInst cl [a, b, c] = Inst2 cl a b c
mkInst cl params = error $ "Unsupported instance:" .- cl .- unwords params

parseInst :: String -> Instance
parseInst x = mkInst cl params
    where (cl : params) = words x
