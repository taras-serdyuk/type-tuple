
module Type.Tuple.Test.Signature where

import Type.Tuple.Test.Text


classFunc :: Int -> String2
classFunc n cl = parens (unwords $ cl:params) .=> foldr1 (.->) params
    where params = take n $ map return ['a' ..]

(.::) :: String3
x .:: y = words3 x "::" y

(.=>) :: String3
x .=> y = words3 x "=>" y

(.->) :: String3
x .-> y = words3 x "->" y
