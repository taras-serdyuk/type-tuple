
module Type.Tuple.Test.Expression where
-- TODO: legacy

import Type.Tuple.Test.Text


mkInst :: String -> [String] -> String -> String
mkInst cl pars res = unwords (cl : pars ++ [res])

-- TODO: refactor
applyInst :: String -> String
applyInst inst = truePhantom $ applyClass cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)

truePhantom :: String2
truePhantom expr = "const True" .- parens expr


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