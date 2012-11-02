{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Interpreter where
-- TODO: rename to Runner or something like that

import Control.Applicative
import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Text


newtype TypeCheck a = TypeCheck { unTypeCheck :: Interpreter a }


instance Functor TypeCheck where
    fmap f = TypeCheck . fmap f . unTypeCheck

instance Applicative TypeCheck where
    pure = TypeCheck . pure
    
    (TypeCheck f) <*> (TypeCheck x) = TypeCheck (f <*> x)

instance Monad TypeCheck where
    return = pure
    
    (TypeCheck x) >>= f = TypeCheck (liftM f x >>= unTypeCheck)

instance MonadIO TypeCheck where
    liftIO = TypeCheck . liftIO


valid, invalid :: String -> String -> Interpreter ()

valid msg expr = void (run expr rethrow) where
    rethrow err = notAllowed (msg .| showInterpErr err)

invalid msg expr = void (run expr (const false) >>= throw) where
    throw True = notAllowed msg
    throw False = false
    false = return False


-- TODO: move truePhantom to here
run :: (MonadInterpreter m) => String -> (InterpreterError -> m Bool) -> m Bool
run = catchError . flip interpret (as :: Bool)

notAllowed :: (MonadError InterpreterError m) => String -> m a
notAllowed = throwError . NotAllowed

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
