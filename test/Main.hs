
module Main where

import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data -- TODO: delete
import Type.Tuple.Test.Runner
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
    run 100 $ same "Head" head NonEmptyTuple
    
    no "Tail () a"
    run 100 $ same "Tail" tail NonEmptyTuple
    
    no "Last () a"
    run 100 $ same "Last" last NonEmptyTuple
    
    no "Init () a"
    run 100 $ same "Init" init NonEmptyTuple
    
    is "Append () () ()"
    run 200 $ same "Append" (++) HalfTuple HalfTuple
    
    is "Length () Zero"
    run 20 $ same "Length" length AnyTuple
    
    run 100 $ same "Drop" drop Nat AnyTuple
