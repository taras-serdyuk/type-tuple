
module Test where

import Control.Monad
import Language.Haskell.Interpreter hiding (parens)


main :: IO ()
main = runInterpreter tests >>= print


tests :: Interpreter ()
tests = do
    loadModules ["Types.hs", "../src/Type/Tuple/List.hs", "../src/Type/Tuple/Tuple.hs"]
    setImports ["Prelude", "Types", "Type.Tuple.Tuple"]
    check "Head" "(A, B)" "B"
    check "Head" "()" "A"

check :: (Functor m, MonadInterpreter m) => String -> String -> String -> m ()
check cl a b = do
    let func = phantom $ constr cl "a" "b" .=> "a" .-> "b"
    let expr = func .- phantom a .:: b
    correct <- typeChecks expr
    unless correct (evalPhantom expr)


type String2 = String -> String
type String3 = String -> String2
type String4 = String -> String3


evalPhantom :: (Functor m, MonadInterpreter m) => String -> m ()
evalPhantom expr = void . eval $ "const ()" .- parens expr

phantom :: String2
phantom a = parens $ "undefined" .:: a


(.::) :: String3
x .:: y = join3 x "::" y

(.=>) :: String3
x .=> y = join3 x "=>" y

(.->) :: String3
x .-> y = join3 x "->" y

constr :: String4
constr cl a b = parens (join3 cl a b)

parens :: String2
parens s = '(' : s ++ ")"

join3 :: String4
join3 x y z = x .- y .- z

(.-) :: String3
x .- y = x ++ " " ++ y
