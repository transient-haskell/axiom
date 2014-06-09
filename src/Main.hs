module Main where
import Haste.DOM
import Haste
import Builder
import View
import Control.Monad.State
import Data.Monoid
import Debug.Trace
import Control.Applicative
import Unsafe.Coerce

(!>) = flip trace

main2= do
  withElem "idelem" . build $
    nelem "hello" `child` (nelem "hello1" `child` nelem "hello3")
--
--    nelem "div" `child`   do
--         ftag "hi" (mempty :: JSBuilder) `attr` ("hi","ho")
--         nelem "ho"

--  return ()



div_ cont=  nelem "div" `child`  cont

main= do
  withElem "idelem" $  ask action
  return ()
  where
  action :: View JSBuilder IO ()
  action = do
     r  <- ((+) <$> inputInt Nothing `triggered` OnKeyUp <++ br
                <*> inputInt Nothing `triggered` OnKeyUp <++ br)
     (nelem "b" `child`  (show r)) ++> noWidget
     return()

br= nelem "br"

triggered :: View JSBuilder IO a -> Event IO b -> View JSBuilder IO a
triggered w event = View $ do
   action <- gets process
   FormElm render mx <- runView  w
   let render' = do
        addEvent (render :: JSBuilder) event $ unsafeCoerce  action
--        addAttr render "autofocus" ""
   return $ FormElm render' mx



ask action e = do
     let iteration= do
          ask action e
     (FormElm render mx, s) <- runStateT (runView action) mFlowState0{process = iteration}
     clearChildren e
     build render e
     return ()




