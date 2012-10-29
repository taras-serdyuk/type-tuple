
module Type.Tuple.Test.Data where

import Data.List
import Type.Tuple.Test.Text
import Type.Tuple.Test.Types


tuple :: String2
tuple [] = "()"
tuple [x] = parens ("Only" .- [x])
tuple xs = parens (intersperse ',' xs)


variantsTill :: Int -> [String]
variantsTill n = concatMap variants [0 .. n]

variants :: Int -> [String]
variants 0 = [""]
variants n = [x:xs | x <- types, xs <- prev]
    where prev = variants (n - 1)
