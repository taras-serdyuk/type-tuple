
module Type.Tuple.Test.Text where


type String2 = String -> String
type String3 = String -> String2
type String4 = String -> String3


parens :: String2
parens s = '(' : s ++ ")"

space3 :: String4
space3 x y z = x .- y .- z

(.-) :: String3
x .- y = x ++ " " ++ y

(.|) :: String3
x .| y = x ++ "\n" ++ y
