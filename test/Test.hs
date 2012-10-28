
module Test where

import Control.Monad
import Language.Haskell.Interpreter hiding (parens)
import Type.Tuple.Test.Phantom


main :: IO ()
main = runInterpreter tests >>= print


tests :: Interpreter ()
tests = do
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    loadModules ["Types.hs", src "List", src "Tuple"]
    setImports ["Prelude", "Types", "Type.Tuple.Tuple"]
    
    check2 "Head" "(A, B)" "A"
    check2 "Head" "()" "A"


check2 :: (Functor m, MonadInterpreter m) => String -> String -> String -> m ()
check2 cl a = check cl [a]

check :: (Functor m, MonadInterpreter m) => String -> [String] -> String -> m ()
check cl pars res = do
    let expr = applyClass cl pars res
    correct <- typeChecks expr
    unless correct (void . eval . showPhantom $ expr)
