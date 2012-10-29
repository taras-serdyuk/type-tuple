
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
    
    
    let inputs = liftIO . applyGen 10 $ inputsGen1 100
    
    invalid $ interpInst "Head" ["()"] "a"
    inputs >>= valids "Head" (return . head)
    
    invalid $ interpInst "Tail" ["()"] "a"
    inputs >>= valids "Tail" (tuple . tail)
    
    invalid $ interpInst "Last" ["()"] "a"
    inputs >>= valids "Last" (return . last)
    
    invalid $ interpInst "Init" ["()"] "a"
    inputs >>= valids "Init" (tuple . init)
    
    valid $ interpInst "Append" ["()", "()"] "()"
    valid $ interpInst "Append" ["()", "Only A"] "Only A"
    valid $ interpInst "Append" ["Only A", "()"] "Only A"


valids :: (Functor m, MonadInterpreter m) => String -> String2 -> [String] -> m ()
valids cl et = mapM_ (\xs -> valid $ interpInst cl [tuple xs] (et xs))
