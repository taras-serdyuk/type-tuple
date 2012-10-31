{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Test where
-- TODO: separate testing DSL and Interpreter API

import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type Class = String


is, no :: String -> Interpreter ()

is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)


eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)

-- TODO: convert to class For
for1 :: (RenderType a, RenderType r, DataGenerator g a) => (Int, Class, a -> r) -> g -> Interpreter ()
for1 (n, cl, et) gen = apply n gen (\x -> valid (mkInst cl [renderType x] (renderType $ et x)) . truePhantom $ applyClass cl [renderType x] (renderType $ et x))

for2 :: (DataGenerator g (a, b), RenderType a, RenderType b, RenderType r) => (Int, Class, a -> b -> r) -> g -> Interpreter ()
for2 (n, cl, et) gen = apply n gen (\(x, y) -> valid (mkInst cl [renderType x, renderType y] (renderType $ et x y)) . truePhantom $ applyClass cl [renderType x, renderType y] (renderType $ et x y))

apply :: (DataGenerator a b, MonadIO m) => Int -> a -> (b -> m c) -> m ()
apply n gen f = liftIO (applyGen n gen) >>= mapM_ f
