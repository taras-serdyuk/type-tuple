
module Test where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Phantom


main :: IO ()
main = runInterpreter tests >>= putStrLn . result where
    result res = case res of
        Left (NotAllowed err) -> err
        Left _ -> "Oops!"
        Right _ -> "Ok"


tests :: Interpreter ()
tests = do
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    loadModules ["Types.hs", src "List", src "Tuple"]
    setImports ["Prelude", "Types", "Type.Tuple.Tuple"]
    
    check2 "Head" "(A, B)" "A"
    check2 "Head" "()" "A"


check2 :: (Functor m, MonadInterpreter m) => String -> String -> String -> m ()
check2 cl a = check cl [a]

check :: (Functor m, MonadInterpreter m) => String -> [String] -> String -> m ()
check cl pars res = catchError (compile expr) rethrow where
    expr = applyClass cl pars res
    compile = void . eval . showPhantom
    rethrow err = throwError $ NotAllowed (inst ++ showError err)
    inst = unwords (cl : pars ++ [res, "\n"])
    showError err = case err of
        WontCompile msgs -> unlines $ map errMsg msgs
        GhcException msg -> msg
        UnknownError msg -> msg
        NotAllowed msg -> msg
