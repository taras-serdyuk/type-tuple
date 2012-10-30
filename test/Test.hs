
module Test where

import Control.Monad
import Language.Haskell.Interpreter
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Test
import Type.Tuple.Test.Text


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
    
    
    let inputsHalf = liftIO . applyGen 5 $ inputsGen 100
    let inputs1 = liftIO . applyGen 10 $ inputsGen1 100
    let inputs = liftIO . applyGen 10 $ inputsGen 100
    
    no "Head () a"
    valids "Head" (return . head) inputs1
    --eq 100 "Head" (return . head) `for1` list'
    
    invalid $ interpInst "Tail" ["()"] "a"
    valids "Tail" (tuple . tail) inputs1
    
    invalid $ interpInst "Last" ["()"] "a"
    valids "Last" (return . last) inputs1
    
    invalid $ interpInst "Init" ["()"] "a"
    valids "Init" (tuple . init) inputs1
    
    is "Append () () ()"
    inputsHalf >>= \xs -> inputsHalf >>= valids2 "Append" (\x y -> tuple (x ++ y)) xs
    
    is "Length () Zero"
    valids "Length" (("Nat" ++) . show . length) inputs

valids :: (Functor m, MonadInterpreter m) => String -> String2 -> m [String] -> m ()
valids cl et xsm = xsm >>= mapM_ (\x -> valid $ interpInst cl [tuple x] (et x))

valids2 :: (Functor m, MonadInterpreter m) => String -> String3 -> [String] -> [String] -> m ()
valids2 cl et = zipWithM_ (\x y -> valid $ interpInst cl [tuple x, tuple y] (et x y))
