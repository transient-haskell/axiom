#!/usr/bin/env execthirdlinedocker.sh

--  mkdir -p ./static && ghcjs -DDEBUG -i../transient/src -i../transient-universe/src -isrc $1 -o static/out && runghc -threaded -i../transient/src -i../transient-universe/src -isrc  $1   ${2} ${3}

{-# LANGUAGE OverloadedStrings #-}
import GHCJS.HPlay.View
import Control.Applicative
import Transient.Base
import Transient.Move
import Data.String
import Prelude hiding (div,span)
import Control.Monad.State


fs= fromString

main =  keep $ initNode $ action2

action2= local $ do
     abduce
     r <-render $  (,,) <$> inputString Nothing 
                        <*> inputString Nothing  
                        <*> inputString Nothing  
                        <**  inputSubmit ("ok" ::String)  `fire` OnClick  

     render $ wraw (h1 (show r)) 
     
size= atr "size"

