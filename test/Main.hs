
module Main where

import Type.Tuple.Test.Data -- TODO: delete
import Type.Tuple.Test.Runner
import Type.Tuple.Test.Test


main :: IO ()
main = do
    putStrLn "Started" 
    result <- run modules imports tests
    print result

modules :: [String]
modules = [src "Nat", src "List", src "Tuple", "Type/Tuple/Test/Types.hs"]
    where src m = "../src/Type/Tuple/" ++ m ++ ".hs"

imports :: [String]
imports = ["Prelude", root "Nat", root "Tuple", root "Test.Types"]
    where root = ("Type.Tuple." ++)


tests :: TypeCheck ()
tests = do
    execute $ putStrLn "Initialized"
    
    no "Head () a"
    test 100 $ same "Head" head NonEmptyTuple
    
    no "Tail () a"
    test 100 $ same "Tail" tail NonEmptyTuple
    
    no "Last () a"
    test 100 $ same "Last" last NonEmptyTuple
    
    no "Init () a"
    test 100 $ same "Init" init NonEmptyTuple
    
    is "Append () () ()"
    test 200 $ same "Append" (++) HalfTuple HalfTuple
    
    is "Length () Zero"
    test 20 $ same "Length" length AnyTuple
    
    test 100 $ same "Drop" drop Nat AnyTuple
