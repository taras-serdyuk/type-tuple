
module Type.Tuple.Test.Base where


data Type a = Type a String


data A = A
data B = B
data C = C
data D = D
data E = E


render :: a -> String
render _ = ""

renderMaybe :: a -> Maybe Char
renderMaybe _ = Nothing
