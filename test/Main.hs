
module Main where

import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data -- TODO: delete
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Test


-- TODO: refactor
main :: IO ()
main = putStrLn "Started" >> runInterpreter (unTypeCheck tests) >>= putStrLn . result where
    result (Left err) = showInterpErr err
    result (Right _) = "Ok"


tests :: TypeCheck ()
tests = do
    -- TODO: extract to setup function
    let test m = "Type/Tuple/Test/" ++ m ++ ".hs"
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    
    TypeCheck $ loadModules [test "Types", src "Nat", src "List", src "Tuple"]
    TypeCheck $ setImports [
        "Prelude",
        "Type.Tuple.Nat",
        "Type.Tuple.Tuple",
        "Type.Tuple.Test.Types"]
    
    liftIO $ putStrLn "Initialized"
    
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
