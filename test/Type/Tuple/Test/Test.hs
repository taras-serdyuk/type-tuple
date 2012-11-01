{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Tuple.Test.Test where

import Language.Haskell.Interpreter -- TODO: delete
import Type.Tuple.Test.Data
import Type.Tuple.Test.Interpreter
import Type.Tuple.Test.Expression


type Class = String

data Renderable = forall a. RenderType a => Renderable a


instance RenderType Renderable where
    renderType (Renderable x) = renderType x


class (DataGenerator g a, RenderType r) => For g a r f | a -> r f where
    for :: (Int, Class, f) -> g -> Interpreter ()
    
    apply :: Int -> Class -> g -> (a -> [Renderable]) -> (a -> r) -> Interpreter ()
    apply n cl gen parsf' resf' = liftIO (applyGen n gen) >>= mapM_ (valid' cl parsf' resf')

-- TODO: simplify
instance (DataGenerator g (Only a), RenderType a, RenderType r) => For g (Only a) r (a -> r) where
    for (n, cl, et) gen = apply n cl gen (return . Renderable) (et . fromOnly)

instance (DataGenerator g (a, b), RenderType a, RenderType b, RenderType r) => For g (a, b) r (a -> b -> r) where
    for (n, cl, et) gen = apply n cl gen (\(x, y) -> [Renderable x, Renderable y]) (uncurry et)


is, no :: String -> Interpreter ()

is inst = valid inst (applyInst inst)
no inst = invalid inst (applyInst inst)


eq :: Int -> Class -> a -> (Int, Class, a)
eq n cl et = (n, cl, et)


-- TODO: refactor
valid' :: (RenderType a, RenderType b) => Class -> (t -> [a]) -> (t -> b) -> t ->Interpreter ()
valid' cl parsf' resf' x = valid (mkInst' cl pars res) (appInst' cl pars res)
    where (pars, res) = (parsf' x, resf' x)

mkInst', appInst' :: (RenderType a, RenderType b) => Class -> [a] -> b -> String
mkInst' cl pars res = mkInst cl (map renderType pars) (renderType res)
appInst' cl pars res = truePhantom $ applyClass cl (map renderType pars) (renderType res)
