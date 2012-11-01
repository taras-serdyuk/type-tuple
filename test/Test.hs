
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
    same 100 "Head" head (for NonEmptyTuple)
    
    no "Tail () a"
    same 100 "Tail" tail (for NonEmptyTuple)
    
    no "Last () a"
    same 100 "Last" last (for NonEmptyTuple)
    
    no "Init () a"
    same 100 "Init" init (for NonEmptyTuple)
    
    is "Append () () ()"
    same 200 "Append" (++) (for HalfTuple HalfTuple)
    
    is "Length () Zero"
    same 20 "Length" length (for AnyTuple)
    
    same 100 "Drop" drop (for Nat AnyTuple)
