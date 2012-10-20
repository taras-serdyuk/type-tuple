
module Type.Tuple.Test.Base where


newtype Type = Type String

data A = A
data B = B
data C = C
data D = D
data E = E


lift :: String -> a
lift = undefined

liftChar :: Char -> a
liftChar = undefined

render :: a -> String
render _ = ""

renderMaybe :: a -> Maybe Char
renderMaybe _ = Nothing
