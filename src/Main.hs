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
     r  <- norefresh ((+) <$> inputInt Nothing `raiseEvent` OnKeyPress <++ br
                         <*> inputInt Nothing `raiseEvent` OnKeyPress <++ br)
     refresh $ (nelem "b" `child`  (show r)) ++> noWidget





