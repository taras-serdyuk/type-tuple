
module Type.Tuple.Test.Test where
-- TODO: separate testing DSL and Interpreter API

import Control.Monad -- TODO: delete
import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Phantom


type Class = String


is, no :: String -> Interpreter ()

is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)

-- TODO: refactor
applyInst :: String -> String
applyInst inst = truePhantom $ applyClass cl pars res where
    (cl : rest) = words inst
    (pars, res) = (init rest, last rest)


-- TODO: convert to class For
for1 :: (RenderType a, RenderType r, DataGenerator g a) => (Int, Class, a -> r) -> g -> Interpreter ()
for1 (n, cl, et) gen = apply1 (mapM_ (\x -> validInterpInstR cl [x] (et x))) (applyGen n gen)

for2 :: (DataGenerator g1 a, DataGenerator g2 b, RenderType a, RenderType b, RenderType r) => (Int, Class, a -> b -> r) -> (g1, g2) -> Interpreter ()
for2 (n, cl, et) (xGen, yGen) = apply2 (zipWithM_ (\x y -> valid (inst cl [renderType x, renderType y] (renderType $ et x y)) . truePhantom $ applyClass cl [renderType x, renderType y] (renderType $ et x y))) (applyGen n xGen) (applyGen n yGen)

inst :: String -> [String] -> String -> String
inst cl pars res = unwords (cl : pars ++ [res])

validInterpInstR :: (RenderType a, RenderType b) => String -> [a] -> b -> Interpreter ()
validInterpInstR cl pars res = valid (inst cl (map renderType pars) (renderType res)) . truePhantom $ applyClass cl (map renderType pars) (renderType res)

apply1 :: (MonadIO m) => (a -> m b) -> IO a -> m b
apply1 f x = liftIO x >>= f

apply2 :: (MonadIO m) => (a -> b -> m c) -> IO a -> IO b -> m c
apply2 f x y = liftIO x >>= \x' -> liftIO y >>= f x'

eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)
