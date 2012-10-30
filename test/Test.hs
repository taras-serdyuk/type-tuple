
module Test where

import Language.Haskell.Interpreter
import Type.Tuple.Test.Data -- TODO: delete
import Type.Tuple.Test.Interpreter -- TODO: delete
import Type.Tuple.Test.Test


main :: IO ()
main = runInterpreter tests >>= putStrLn . result where
    result (Left err) = showInterpErr err
    result (Right _) = "Ok"


tests :: Interpreter ()
tests = do
    let test m = "Type/Tuple/Test/" ++ m ++ ".hs"
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    
    loadModules [test "Types", src "Nat", src "List", src "Tuple"]
    setImports [
        "Prelude",
        "Type.Tuple.Nat",
        "Type.Tuple.Tuple",
        "Type.Tuple.Test.Types"]
    
    
    no "Head () a"
    eq 50 "Head" head `for1` list'
    
    no "Tail () a"
    eq 50 "Tail" tail `for1` list'
    
    no "Last () a"
    eq 50 "Last" last `for1` list'
    
    no "Init () a"
    eq 50 "Init" init `for1` list'
    
    is "Append () () ()"
    eq 200 "Append" (++) `for2` (list2, list2)
    
    is "Length () Zero"
    eq 20 "Length" length `for1` list
