
module Test where

import Language.Haskell.Interpreter
import Type.Tuple.Test.Interpreter


main :: IO ()
main = runInterpreter tests >>= putStrLn . result where
    result (Left err) = showInterpErr err
    result (Right _) = "Ok"


tests :: Interpreter ()
tests = do
    let test m = "Type/Tuple/Test/" ++ m ++ ".hs"
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    loadModules [test "Types", src "List", src "Tuple"]
    setImports [
        "Prelude",
        "Type.Tuple.Tuple",
        "Type.Tuple.Test.Types"]
    
    valid $ interpInst "Head" ["(A, B)"] "A"
    invalid $ interpInst "Head" ["(A, B)"] "A"
    invalid $ interpInst "Head" ["()"] "A"
