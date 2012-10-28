
module Test where

import Language.Haskell.Interpreter
import Type.Tuple.Test.Interpreter


main :: IO ()
main = runInterpreter tests >>= putStrLn . result where
    result (Left err) = showInterpErr err
    result (Right _) = "Ok"


tests :: Interpreter ()
tests = do
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    loadModules ["Types.hs", src "List", src "Tuple"]
    setImports ["Prelude", "Types", "Type.Tuple.Tuple"]
    
    valid $ interpInst "Head" ["(A, B)"] "A"
    invalid $ interpInst "Head" ["(A, B)"] "A"
    invalid $ interpInst "Head" ["()"] "A"
    return ()
