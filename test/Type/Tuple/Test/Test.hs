
module Type.Tuple.Test.Test where
-- TODO: separate testing DSL and Interpreter API

import Control.Applicative
import Control.Monad
import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type Class = String


is, no :: String -> Interpreter ()

is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)


-- TODO: convert to class For
for1 :: (RenderType a, RenderType r, DataGenerator g a) => (Int, Class, a -> r) -> g -> Interpreter ()
for1 (n, cl, et) gen = apply1 (mapM_ (\x -> validInterpInstR cl [x] (et x))) (applyGen n gen)

for2 :: (DataGenerator g1 a, DataGenerator g2 b, RenderType a, RenderType b, RenderType r) => (Int, Class, a -> b -> r) -> (g1, g2) -> Interpreter ()
for2 (n, cl, et) (xGen, yGen) = apply2 (mapM_ (\(x, y) -> valid (mkInst cl [renderType x, renderType y] (renderType $ et x y)) . truePhantom $ applyClass cl [renderType x, renderType y] (renderType $ et x y))) (applyGen n xGen) (applyGen n yGen) where



validInterpInstR :: (RenderType a, RenderType b) => String -> [a] -> b -> Interpreter ()
validInterpInstR cl pars res = valid (mkInst cl (map renderType pars) (renderType res)) . truePhantom $ applyClass cl (map renderType pars) (renderType res)

apply1 :: (MonadIO m) => ([a] -> m c) -> IO [a] -> m c
apply1 f x = liftIO x >>= f

-- TODO: refactor using composite generator from Data
apply2 :: (Applicative m, MonadIO m) => ([(a, b)] -> m c) -> IO [a] -> IO [b] -> m c
apply2 f x y = zip <$> liftIO x <*> liftIO y >>= f
apply2 f x y = liftM2 zip (liftIO x) (liftIO y) >>= f

eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)
