{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Runner where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Text


data Result = Ok | Error String

newtype TypeCheck a = TypeCheck { unTypeCheck :: Interpreter a }


instance Show Result where
    show Ok = "Ok"
    show (Error msg) = "Error" .| msg

instance Monad TypeCheck where
    return = TypeCheck . return
    
    (TypeCheck x) >>= f = TypeCheck $ x >>= (unTypeCheck . f)


run :: [String] -> [String] -> TypeCheck () -> IO Result
run modules imports tests = fmap mkResult . runInterpreter $ do
    loadModules modules
    setImports imports
    unTypeCheck tests

mkResult :: Either InterpreterError () -> Result
mkResult = either (Error . showInterpErr) (const Ok)

execute :: IO a -> TypeCheck a
execute = TypeCheck . liftIO


valid, invalid :: String -> String -> Interpreter ()

valid msg expr = void (interp expr rethrow) where
    rethrow err = notAllowed (msg .| showInterpErr err)

invalid msg expr = void (interp expr (const false) >>= throw) where
    throw True = notAllowed msg
    throw False = false
    false = return False


-- TODO: move truePhantom to here
interp :: (MonadInterpreter m) => String -> (InterpreterError -> m Bool) -> m Bool
interp = catchError . flip interpret (as :: Bool)

notAllowed :: (MonadError InterpreterError m) => String -> m a
notAllowed = throwError . NotAllowed

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
