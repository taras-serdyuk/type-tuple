
module Type.Tuple.Test.Basic where

import Type.Tuple.Test.Base
import Type.Tuple.Test.Dummy as D


propAppend :: (D.Append a b c) => Type a -> Type b -> Bool
propAppend (Type x x') (Type y y') = render (D.append x y) == (x' ++ y')


propHead :: (Head a b) => Type a -> Bool
propHead (Type x x') = renderMaybe (D.head x) == headMaybe x'

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x
