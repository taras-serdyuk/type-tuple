{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Type.Tuple.Test.Test where

import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type Class = String
data Renderable = forall a. RenderType a => Renderable a


instance RenderType Renderable where
    renderType (Renderable x) = renderType x

render :: (RenderType a) => a -> Renderable
render = Renderable


is, no :: String -> Interpreter ()

is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)


eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)

-- TODO: convert to class For
for1 :: (RenderType a, RenderType r, DataGenerator g a) => (Int, Class, a -> r) -> g -> Interpreter ()
for1 (n, cl, et) gen = apply n cl gen pars res where
    pars x = [x]
    res = et

-- TODO: replace tuple with 2 parameters
for2 :: (DataGenerator g (a, b), RenderType a, RenderType b, RenderType r) => (Int, Class, a -> b -> r) -> g -> Interpreter ()
for2 (n, cl, et) gen = apply n cl gen pars res  where
    pars (x, y) = [render x, render y]
    res = uncurry et


apply :: (DataGenerator a b, RenderType c, RenderType d) => Int -> Class -> a -> (b -> [c]) -> (b -> d) -> Interpreter ()
apply n cl gen parsf resf = liftIO (applyGen n gen) >>= mapM_ (\x -> valid (mkInst' cl (parsf x) (resf x)) $ appInst' cl (parsf x) (resf x))

mkInst', appInst' :: (RenderType a, RenderType b) => Class -> [a] -> b -> String
mkInst' cl pars res = mkInst cl (map renderType pars) (renderType res)
appInst' cl pars res = truePhantom $ applyClass cl (map renderType pars) (renderType res)