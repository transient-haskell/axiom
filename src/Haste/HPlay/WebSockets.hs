-----------------------------------------------------------------------------
--
-- Module      :  Haste.HPlay.WebSockets
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
{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}
module Haste.HPlay.WebSockets(
wsOpen, wsAsk, wsClose
)where

import Haste.HPlay.View
--import Haste.HPlay.WebSockets
--import Haste.WebSockets
import Haste
import Haste.Foreign
import Unsafe.Coerce
import System.IO.Unsafe
import Data.IORef
import Control.Monad.IO.Class
import Data.List (nubBy)


data WebSocket
instance Pack WebSocket where
  pack = unsafeCoerce

instance Unpack WebSocket where
  unpack = unsafeCoerce

newtype WSComputation = WSComputation (WebSocket -> IO ())

instance Pack WSComputation where
  pack = unsafeCoerce

instance Unpack WSComputation where
  unpack = unsafeCoerce

newtype WSOnError = WSOnError (IO ())
instance Pack WSOnError where
  pack = unsafeCoerce
instance Unpack WSOnError where
  unpack = unsafeCoerce

newtype WSOnMsg = WSOnMsg (WebSocket -> JSString -> IO ())
instance Pack WSOnMsg where
  pack = unsafeCoerce
instance Unpack WSOnMsg where
  unpack = unsafeCoerce

type SockId = String

rsockets :: IORef [(String,(WebSocket,Maybe JSString))]
rsockets= unsafePerformIO $ newIORef []

wsClose :: SockId -> Widget ()
wsClose id= View $ do
  wss <- liftIO $ readIORef rsockets
  case lookup id wss of
    Nothing -> return $ FormElm noHtml $ Just ()

    Just (ws,_) -> do
        cont <- getCont

        let onclose= WSOnError $  do
                             writeIORef rsockets $ filter ( (/= id) . fst)  wss
                             runCont cont

        liftIO $ closes ws onclose
        return $ FormElm noHtml Nothing
  where
  closes :: WebSocket -> WSOnError -> IO()
  closes ws onclose= ffi "(function(ws){\
                        \ws.onclose = function(ws,onclose) {B(A(onclose,[0]));};\
                        \ws.close()})"

wsOpen :: URL ->  Widget SockId
wsOpen url = View $ do
  id <- genNewId
  wss <- liftIO $ readIORef rsockets
  case lookup id wss of
    Just _ -> return $ FormElm noHtml $ Just id

    Nothing -> do
        cont <- getCont

        let onopen= WSComputation $ \ws -> do
                             writeIORef rsockets $(id, (ws,Nothing)):wss
                             runCont cont

        liftIO $ news url  onopen -- $ WSOnError $ error "WebSocket closed unexpectedly"
        return $ FormElm noHtml Nothing


    where
    news :: URL
         -> WSComputation
    --     -> WSOnError
         -> IO ()
    news= ffi $  "(function(url,f){\
             \var ws = new WebSocket(url);\
             \ws.onopen = function(e) {B(A(f,[ws,0]));};\
             \return ws;\
           \})"

wsAsk :: (JSType a,JSType b) => SockId -> a -> Widget b
wsAsk id x = View $ do
   cont <- getCont
   wss <- liftIO $ readIORef rsockets
   case lookup id wss of
       Nothing -> error "socket not opened"
       Just (s,Just r) -> do
            liftIO $ writeIORef rsockets $ nubBy (\s s' -> fst s== fst s') $ (id, (s, Nothing)):wss
            return $ FormElm noHtml $ fromJSString r
       Just (ws',Nothing) -> do
            liftIO $ snd  ws' ( toJSString x) $ WSOnMsg $ \s resp -> do
                        writeIORef rsockets $ nubBy (\s s' -> fst s== fst s') $ (id, (s, Just resp)):wss
                        runCont cont
            return $ FormElm noHtml Nothing
   where
   snd :: WebSocket -> JSString -> WSOnMsg -> IO ()
   snd= ffi $  "(function(s, msg, cb) {\n\
                    \s.onmessage= function(e) {B(A(cb,[s, [0,e.data],0]));};\n\
                    \s.send(msg);})\n"


