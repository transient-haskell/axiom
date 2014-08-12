-----------------------------------------------------------------------------
--
-- Module      :  Haste.HPlay.Console
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Haste.HPlay.Console (
lit, print, putStrLn, putStr, getChar,getLine
) where
import Haste.HPlay.View
import Prelude hiding (span,print, putStrLn, putStr,getChar,getLine,div)
import Control.Monad.IO.Class
import Haste
import Control.Concurrent.MVar

lit html= runBody $ wraw $ nelem "div" `setHtml` html

print x=  getConsole >>= build  (span (show x) >> br)

putStrLn x =  getConsole >>= build (span x >> br)

putStr x=  getConsole >>= build (span x)



getConsole= do
      me <- elemById  "console"
      case me of
        Nothing ->do
           body <- getBody
           build (div !  atr "id" "console" $ noHtml) body
        Just e -> return e

getChar :: IO Char
getChar=  do
   con <- getConsole
   Just ch <- (flip runWidget) con $ do
       c <- getString Nothing `fire` OnKeyUp
--       wraw $ this >> delete
       return c


   return $ Prelude.head ch


getLine :: IO String
getLine=  do
   con <- getConsole
   tv <- newEmptyMVar
   runWidget (loop tv)  con
   str <- takeMVar tv
   return str
   where
   loop tv= do
       s <- getString Nothing `fire` OnKeyUp
       EventData ev dat <- getEventData
       if ( dat==  Key 13) then alert s >>  liftIO (putMVar tv s) else loop tv

   getString1 = do
      mid <- getSessionData
      case mid of
        Nothing -> do
            i <-getNextId
            setSData i
            getString Nothing `fire`  OnKeyUp
        Just id ->  do
          Just e <- elemById id
          (View $ do
            v <- getAttr e "value"
            return $ FormElm (Perch$ const $ return e) $ Just v) --  `fire` OnKeyUp


