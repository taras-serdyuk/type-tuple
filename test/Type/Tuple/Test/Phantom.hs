
module Type.Tuple.Test.Phantom where

import Type.Tuple.Test.Text


showPhantom :: String -> String
showPhantom expr = "const ()" .- parens expr

applyClass :: String -> [String] -> String2
applyClass cl pars res = unwords (func : map phantom pars) .:: res
    where func = phantom $ classFunc (length pars + 1) cl

phantom :: String2
phantom a = parens $ "undefined" .:: a


classFunc :: Int -> String2
classFunc n cl = parens (unwords $ cl:params) .=> foldr1 (.->) params
    where params = take n $ map return ['a' ..]

(.::) :: String3
x .:: y = space3 x "::" y

(.=>) :: String3
x .=> y = space3 x "=>" y

(.->) :: String3
x .-> y = space3 x "->" y
