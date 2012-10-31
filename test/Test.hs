
module Test where

import Language.Haskell.Interpreter
import Type.Tuple.Test.Data -- TODO: delete
import Type.Tuple.Test.Interpreter -- TODO: delete
import Type.Tuple.Test.Test


-- TODO: refactor
main :: IO ()
main = runInterpreter tests >>= putStrLn . result where
    result (Left err) = showInterpErr err
    result (Right _) = "Ok"


tests :: Interpreter ()
tests = do
    -- TODO: extract
    let test m = "Type/Tuple/Test/" ++ m ++ ".hs"
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    
    loadModules [test "Types", src "Nat", src "List", src "Tuple"]
    setImports [
        "Prelude",
        "Type.Tuple.Nat",
        "Type.Tuple.Tuple",
        "Type.Tuple.Test.Types"]
    
    
    no "Head () a"
    eq 100 "Head" head `for1` NonEmptyTuple
    
    no "Tail () a"
    eq 100 "Tail" tail `for1` NonEmptyTuple
    
    no "Last () a"
    eq 100 "Last" last `for1` NonEmptyTuple
    
    no "Init () a"
    eq 100 "Init" init `for1` NonEmptyTuple
    
    is "Append () () ()"
    eq 200 "Append" (++) `for2` Pair HalfTuple HalfTuple
    
    is "Length () Zero"
    eq 20 "Length" length `for1` AnyTuple
    
    eq 100 "Drop" drop `for2` Pair Nat AnyTuple
