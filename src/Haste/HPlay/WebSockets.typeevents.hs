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
-- | Hplayground WebSockets have de-inversion of control. it uses wsAsk for syncronous
-- request-responses.
--
-- wsAsk can also handle asynchronous traffic too since it catches every received
-- message, not only the next message
--
-- For an example, see <http://tryplayg.herokuapp.com/try/hplay-sockets.hs/edit>
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




type SockId = String

type Index= Int

rsockets :: IORef [(String,(WebSocket,Index,[EventF],Maybe JSString))]
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

-- | open the socket and continue the flow when it is opened.
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

        withWebSockets url  ondata onerror  onopen -- liftIO $ news url  onopen
        return $ FormElm noHtml Nothing

ondata resp = do
    wss <- liftIO $ readIORef rsockets
    case lookup id wss of
       Nothing -> error "socket not opened"
       Just (s,index,handlers,_) -> search  handlers index
    where
    search handlers index = do
        let get = handlers !! index
        mr <- handle (const $ return Nothing) get resp
        case mr of
          Nothing -> search handlers $ if index == lenght handlers then 0 else index + 1
          Just r  -> do
             writeIORef rsockets $ nubBy (\s s' -> fst s== fst s') $ (id, (s,index,handlers, Just r)):wss
             runCont cont

--    where
--    news :: URL
--         -> WSComputation
--    --     -> WSOnError
--         -> IO ()
--    news= ffi $  "(function(url,f){\
--             \var ws = new WebSocket(url);\
--             \ws.onopen = function(e) {B(A(f,[ws,0]));};\
--             \return ws;\
--           \})"

-- | syncronous request-responses.
-- wsAsk can also handle asynchronous traffic too since it catches every received
-- message, not only the next message and executes the rest of the computation
-- as is usual in a active widget
--
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


