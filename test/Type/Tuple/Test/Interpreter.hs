{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Interpreter where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Phantom
import Type.Tuple.Test.Text


valid, invalid :: (Functor m, MonadInterpreter m) => (String, m Bool) -> m ()

valid (inst, interp) = void $ catchError interp rethrow where
    rethrow err = failInst inst (showInterpErr err)

invalid (inst, interp) = void $ catchError interp (const false) >>= throw where
    throw True = failInst inst "Compiled!"
    throw False = false
    false = return False


interpInst :: (MonadInterpreter m) => String -> [String] -> String -> (String, m Bool)
interpInst cl pars res = (inst, interp) where
    inst = unwords (cl : pars ++ [res])
    interp = interpret expr (as :: Bool)
    expr = truePhantom (applyClass cl pars res)

failInst :: (MonadError InterpreterError m) => String -> String -> m a
failInst inst msg = throwError $ NotAllowed (inst .| msg)

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
