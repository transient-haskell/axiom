#!/usr/bin/env execthirdlinedocker.sh
--  info: use sed -i 's/\r//g' file if report "/usr/bin/env: ‘execthirdlinedocker.sh\r’: No such file or directory"
-- LIB="/projects" && ghcjs --make   -i${LIB}/transient/src -i${LIB}/transient-universe/src  -i${LIB}/axiom/src   $1 -o static/out && runghc   -DDEBUG  -i${LIB}/transient/src -i${LIB}/transient-universe/src -i${LIB}/axiom/src   $1 ${2} ${3}


import GHCJS.HPlay.View
import Control.Applicative
import Transient.Base
import Transient.Move

main =  keep $ initNode $ action

action :: Cloud ()
action = local $ do
  r <- render $ wbutton 10 (toJSString "try this")
  render $ wraw (h1 (show r))
