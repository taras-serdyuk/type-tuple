
module Type.Tuple.Test.Basic where

import Data.List as L
import Type.Tuple.Test.Base



appendTest = test $ do
    x <- types; y <- types
    Actual (fun2 "D.append" x y) `vs` Expected (x ++ y)

headTest = test $ do
    x <- filter (not . null) types
    Actual (fun1 "D.head" x) `vs` Expected [head x]

headFail = test [fun1 "D.head" ""]


types = ["", "A", "B", "AA", "AB", "BB", "AAA"]

data Result = Actual String | Expected String
type Tup = String

fun1 :: String -> Tup -> String
fun1 fun x = fun ++ " " ++ lift x

fun2 :: String -> Tup -> Tup -> String
fun2 fun x y = fun1 fun x ++ " " ++ lift y


vs :: Result -> Result -> [String]
vs (Actual x) (Expected y) = ["check (" ++ x ++ ") :: " ++ lift y]

test :: [String] -> String
test = unlines . intersperse "$"

