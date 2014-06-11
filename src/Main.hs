module Main where
import Haste
import Haste.DOM(withElem)
import View
import Builder
import Control.Applicative



main= do
  withElem "idelem" $  runWidget action
  return ()
  where
  action :: View JSBuilder IO ()
  action = do
     r <- norefresh $ (+)
            <$> fromStr "first number  "
            ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
            <*> fromStr "second number "
            ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
     refresh $ fromStr "result: " ++>  b (show r) ++> noWidget


nelemc :: ToElem a => String ->  a -> JSBuilder
nelemc tag cont= nelem tag `child` cont

b= nelemc "b"

(<<) ce cont= ce cont

