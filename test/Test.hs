
module Test where

import Control.Monad
import Language.Haskell.Interpreter hiding (parens)
import Type.Tuple.Test.Signature
import Type.Tuple.Test.Text


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
    let func = phantom $ classFunc 2 cl
    let expr = func .- phantom a .:: b
    correct <- typeChecks expr
    unless correct (evalPhantom expr)


evalPhantom :: (Functor m, MonadInterpreter m) => String -> m ()
evalPhantom expr = void . eval $ "const ()" .- parens expr

phantom :: String2
phantom a = parens $ "undefined" .:: a
