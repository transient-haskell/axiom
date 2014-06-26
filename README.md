Playground
==========
Create applications in the browser as fast as easy as console applications and have reactive, window-oriented
and spreadsheet-like behaviours for free.

So you translate your inputs and outputs from console calls to playground widgets and with no more modifucations
you have reactive and spreadsheet behaviours.

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

An online example is here, with some explanations:

http://mflowdemo.herokuapp.com/noscript/wiki/browserwidgets

This is the same example running, inserted as a script:

<div id="idelem"></div>
<script  src="http://mflowdemo.herokuapp.com/browserwidget.js" type "text/javascript"></script>


The source of the last version of this example is the Main.hs:

https://github.com/agocorona/playground/blob/master/src/Main.hs
How it works
============
Under the hood there is the good old formlet concept. It uses monadic and applicative combinators
The very same ones used by MFlow in the server side. While the server side widgets of MFlow
produce blaze-html output converted to bytestrings, playground construct a builder function that
creates a tree in the HTML DOM when executed. This builder (perch) is monoidal so the formlet
can aggregate subtrees. When some event happens in the widget subtree, the widget executes
his code and reconstruct itself. If it return a valid result and it is in a monadic computation
the tree continues recreating itself downstream by executing further widgets in the monadic sequence.
If the event is raised within a widget that does not generate a valid result (return empty)
the remaining widgets continue unchanged and unevaluated.

Status
======

Propagation upstream can be achieved with the witerate modifier, used also in the server side
(not tested). A higuer level of spreadsheet like behaviour can be simulated with the cell
abstraction (not yet finished) but the current example show behaviours typical of spreadsheets
and window aplications as well as console applications.

