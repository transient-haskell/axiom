haskell-js-reactive-widgets
===========================

Reactive haskell widgets running in the browser. The MFlow widgets running in the browser with the Haste compiler

This program creates two input boxes and present the sum below them:


        import Haste
        import Haste.DOM(withElem)
        import View
        import Builder
        import Control.Applicative


        main= do
          withElem "idelem" $ runWidget action
          return ()

        action :: View JSBuilder IO ()
        action = do
             r  <- norefresh ((+) <$> inputInt Nothing `raiseEvent` OnKeyPress <++ br
                                 <*> inputInt Nothing `raiseEvent` OnKeyPress <++ br)
             refresh $ (nelem "b" `child`  (show r)) ++> noWidget

This program creates his own rendering, that can be changed dinamically.



