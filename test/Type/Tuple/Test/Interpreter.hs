{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Interpreter where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Text


valid, invalid :: (Functor m, MonadInterpreter m) => String -> String -> m ()

valid msg expr = void $ run expr rethrow where
    rethrow err = notAllowed (msg .| showInterpErr err)

invalid msg expr = void $ run expr (const false) >>= throw where
    throw True = notAllowed msg
    throw False = false
    false = return False


run :: MonadInterpreter m => String -> (InterpreterError -> m Bool) -> m Bool
run expr = catchError (interpret expr (as :: Bool))

notAllowed :: (MonadError InterpreterError m) => String -> m a
notAllowed = throwError . NotAllowed

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
