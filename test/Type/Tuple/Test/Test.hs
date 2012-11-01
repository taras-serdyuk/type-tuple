{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Type.Tuple.Test.Test where

import Control.Applicative
import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type Class = String
type ForF f = (Int, Class, f) -> Interpreter ()

data Renderable = forall a. RenderType a => Renderable a


instance RenderType Renderable where
    renderType (Renderable x) = renderType x


class For f where
    for :: f

-- TODO: refactor
instance (DataGenerator g a, RenderType a, RenderType r) => For (g -> ForF (a -> r)) where
    for gen (n, cl, et) = liftIO (applyGen n gen) >>= mapM_ (valid' cl (return . Renderable) et)

-- TODO: refactor
instance (DataGenerator g a, DataGenerator j b, RenderType a, RenderType b, RenderType r) => For (g -> j -> ForF (a -> b -> r)) where
    for gen jen (n, cl, et) = liftIO (applyGen1 n ((,) <$> generator gen <*> generator jen)) >>= mapM_ (valid' cl (\(x, y) -> [Renderable x, Renderable y]) (uncurry et))


is, no :: String -> Interpreter ()

is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)


eq :: Int -> Class -> a -> ForF a -> Interpreter ()
eq n cl et f = f (n, cl, et)


-- TODO: refactor
valid' :: (RenderType a, RenderType b) => Class -> (t -> [a]) -> (t -> b) -> t ->Interpreter ()
valid' cl parsf' resf' x = valid (mkInst' cl pars res) (appInst' cl pars res)
    where (pars, res) = (parsf' x, resf' x)

mkInst', appInst' :: (RenderType a, RenderType b) => Class -> [a] -> b -> String
mkInst' cl pars res = mkInst cl (map renderType pars) (renderType res)
appInst' cl pars res = truePhantom $ applyClass cl (map renderType pars) (renderType res)
