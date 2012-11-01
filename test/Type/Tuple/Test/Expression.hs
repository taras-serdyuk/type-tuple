
module Type.Tuple.Test.Expression where
-- TODO: legacy

import Type.Tuple.Test.Data -- TODO: delete
import Type.Tuple.Test.Text


-- TODO: Combine sections with For class
type Class = String

data Instance =
    Inst1 Class String String |
    Inst2 Class String String String


instance RenderType Instance where
    renderType x = unwords (instClass x : instParams x ++ [instResult x])


instClass :: Instance -> String
instClass (Inst1 cl _ _) = cl
instClass (Inst2 cl _ _ _) = cl

instParams :: Instance -> [String]
instParams (Inst1 _ x _) = [x]
instParams (Inst2 _ x y _) = [x, y]

instResult :: Instance -> String
instResult (Inst1 _ _ x) = x
instResult (Inst2 _ _ _ x) = x


mkInst :: Class -> [String] -> Instance
mkInst cl [a, b] = Inst1 cl a b
mkInst cl [a, b, c] = Inst2 cl a b c
mkInst cl pars = error $ "Unsupported instance:" .- cl .- unwords pars


parseInst :: String -> Instance
parseInst x = mkInst cl pars
    where (cl : pars) = words x

applyInst :: Instance -> String
applyInst x = truePhantom $ unwords (func : map phantom (instParams x)) .:: instResult x
    where func = phantom $ classFunc (length (instParams x) + 1) (instClass x)

truePhantom :: String2
truePhantom expr = "const True" .- parens expr

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
