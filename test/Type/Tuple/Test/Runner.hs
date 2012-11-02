{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Runner where

import Control.Monad.Error
import Language.Haskell.Interpreter
import Type.Tuple.Test.Expression
import Type.Tuple.Test.Text


type Interp = Interpreter ()

newtype Test a = Test { unTest :: Interpreter a }

data Result = Ok | Error String


instance Show Result where
    show Ok = "Ok"
    show (Error msg) = "Error" .| msg

instance Monad Test where
    return = Test . return
    
    (Test x) >>= f = Test $ x >>= (unTest . f)


run :: [String] -> [String] -> Test () -> IO Result
run modules imports tests = fmap mkResult . runInterpreter $ do
    loadModules modules
    setImports imports
    unTest tests

mkResult :: Either InterpreterError () -> Result
mkResult = either (Error . showInterpErr) (const Ok)

execute :: IO a -> Test a
execute = Test . liftIO


valid, invalid :: String -> String -> Interp

valid msg expr = void (interp expr rethrow) where
    rethrow err = notAllowed (msg .| showInterpErr err)

invalid msg expr = void (interp expr (const false) >>= throw) where
    throw True = notAllowed msg
    throw False = false
    false = return False


interp :: (MonadInterpreter m) => String -> (InterpreterError -> m Bool) -> m Bool
interp = catchError . flip interpret (as :: Bool) . constExpr "True"

notAllowed :: (MonadError InterpreterError m) => String -> m a
notAllowed = throwError . NotAllowed

showInterpErr :: InterpreterError -> String
showInterpErr err = case err of
    WontCompile msgs -> unlines $ map errMsg msgs
    GhcException msg -> msg
    UnknownError msg -> msg
    NotAllowed msg -> msg
