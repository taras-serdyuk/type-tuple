
module Test where

import Language.Haskell.Interpreter
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Text


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
    
    invalid $ interpInst "Head" ["()"] "a"
    valids "Head" tail (return . head)
    
    invalid $ interpInst "Tail" ["()"] "a"
    valids "Tail" tail (tuple . tail)
    
    invalid $ interpInst "Last" ["()"] "a"
    valids "Last" tail (return . last)


valids :: (Functor m, MonadInterpreter m) => String -> ([String] -> [String]) -> String2 -> m ()
valids cl filt et = mapM_ test (filt inputs) where
    test xs = valid $ interpInst cl [tuple xs] (et xs)
    inputs = variantsTill 5
