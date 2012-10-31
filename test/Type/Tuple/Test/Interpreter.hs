{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Interpreter where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Text


valid, invalid :: String -> String -> Interpreter ()

valid msg expr = void $ run expr rethrow where
    rethrow err = incorrect msg (showInterpErr err)

invalid msg expr = void $ run expr (const false) >>= throw where
    throw True = incorrect msg "Compiled!"
    throw False = false
    false = return False


run :: MonadInterpreter m => String -> (InterpreterError -> m Bool) -> m Bool
run expr = catchError (interpret expr (as :: Bool))

incorrect :: (MonadError InterpreterError m) => String -> String -> m a
incorrect msg ctx = throwError $ NotAllowed (msg .| ctx)

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
