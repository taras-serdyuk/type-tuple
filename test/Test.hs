
module Test where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Phantom


main :: IO ()
main = runInterpreter tests >>= putStrLn . message where
    message res = case res of
        Left (NotAllowed err) -> err
        Left err -> "Oops!\n" ++ showInterpErr err
        Right _ -> "Ok"


tests :: Interpreter ()
tests = do
    let src m = "../src/Type/Tuple/" ++ m ++ ".hs"
    loadModules ["Types.hs", src "List", src "Tuple"]
    setImports ["Prelude", "Types", "Type.Tuple.Tuple"]
    
    check2 "Head" "(A, B)" "A"
    --notcheck2 "Head" "(A, B)" "A"
    notcheck2 "Head" "()" "A"


check2 :: (Functor m, MonadInterpreter m) => String -> String -> String -> m ()
check2 cl a = check cl [a]

check :: (Functor m, MonadInterpreter m) => String -> [String] -> String -> m ()
check cl pars res = catchError (compile expr) rethrow where
    expr = applyClass cl pars res
    compile = void . eval . truePhantom
    rethrow err = throwError $ NotAllowed (inst ++ showInterpErr err)
    inst = unwords (cl : pars ++ [res, "\n"])
    

notcheck2 :: (Functor m, MonadInterpreter m) => String -> String -> String -> m ()
notcheck2 cl a = notcheck cl [a]

notcheck :: (Functor m, MonadInterpreter m) => String -> [String] -> String -> m ()
notcheck cl pars res = catchError compile suppress >>= (\comp -> when comp (throwError err)) where
    expr = applyClass cl pars res
    compile = interpret (truePhantom expr) (as :: Bool)
    suppress _ = return False
    err = NotAllowed (inst ++ "\nCompiled!") -- Distinguish my error or change structure + refactor with check
    inst = unwords (cl : pars ++ [res, "\n"])

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
