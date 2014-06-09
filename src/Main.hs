module Main where
import Haste
import Haste.DOM(withElem)
import View
import Builder
import Control.Applicative



main= do
  withElem "idelem" $  ask action
  return ()
  where
  action :: View JSBuilder IO ()
  action = do
     r  <- norefresh ((+) <$> inputInt Nothing `eventOn` OnKeyPress <++ br
                         <*> inputInt Nothing `eventOn` OnKeyPress <++ br)
     refresh $ (nelem "b" `child`  (show r)) ++> noWidget





