import GHCJS.HPlay.View
import Control.Applicative
import Transient.Base
import Transient.Move

main =  keep $ initNode $ action

action :: Cloud ()
action = local $ do
  r <- render $ wbutton 10 (toJSString "try this")
  render $ wraw (h1 (show r))
