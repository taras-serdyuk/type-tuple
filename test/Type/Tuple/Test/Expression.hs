
module Type.Tuple.Test.Expression where

import Type.Tuple.Test.Instance
import Type.Tuple.Test.Text


applyInst :: Instance -> String
applyInst x = unwords (func : map phantom params) .:: instResult x where
    func = phantom $ classFunc (length params + 1) (instClass x)
    params = instParams x

constExpr :: String3
constExpr value expr = "const" .- value .- parens expr

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
