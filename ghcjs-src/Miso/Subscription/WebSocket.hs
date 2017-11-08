{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.WebSocket
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.WebSocket
  ( -- * Types
    WebSocket
  , WebSocketEvent       (..)
  , WebSocketClosedEvent (..)
  , SocketState          (..)
  , CloseCode            (..)

    -- * Subscription
  , websocketSub
  , websocketSubWithCloseHandler

    -- * Creating a websocket
  , newWebSocket

    -- * Interacting with the websocket
  , sendJsonToWebSocket
  , sendTextToWebSocket
  , closeWebSocket
  , closeWebSocketWithCode

    -- * Querying the socket
  , getSocketState
  , getSocketURL

    -- * Handling a close event
  , reconnectIfAbnormal
  ) where

import Prelude
import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad
import Data.Aeson
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import Prelude                 hiding (map)

import Miso.FFI                (parse, stringify)
import Miso.Html.Internal      (Sub)
import Miso.String

-- | High-level WebSocket datatype.
-- Wraps the low-level value in an IORef to hide reconnection.
data WebSocket = WebSocket {
  getSocketURL :: !MisoString,
  wsProtocols :: ![MisoString],
  wsSocketRef :: !(IORef WebSocket_)
  } deriving (Eq)

-- | WebSocket_ data type, wrapper around the JSVal.
newtype WebSocket_ = WebSocket_ JSVal

instance Show WebSocket where
  show ws = "WebSocket(" <> show (getSocketURL ws) <> ")"

instance Eq WebSocket_ where
  (==) = webSocketsEqual'

-- | WebSocket connection messages. The `message` type should be a
-- | `FromJSON` instance.
data WebSocketEvent message
  = WebSocketMessage message
  | WebSocketClosed WebSocketClosedEvent
  | WebSocketOpen
  | WebSocketError MisoString
  deriving (Show, Eq, Generic)

-- | Information received when a WebSocket closes.
data WebSocketClosedEvent = WebSocketClosedEvent {
  closedCode :: CloseCode,
  closedCleanly :: Bool,
  closedReason :: MisoString
  }
  deriving (Show, Eq, Generic)

-- | `SocketState` corresponding to current WebSocket connection
data SocketState
  = WEBSOCKET_CONNECTING -- ^ 0
  | WEBSOCKET_OPEN       -- ^ 1
  | WEBSOCKET_CLOSING    -- ^ 2
  | WEBSOCKET_CLOSED     -- ^ 3
  deriving (Show, Eq, Ord, Enum)

-- | Code corresponding to a closed connection
-- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent
data CloseCode
  = Normal_Closure
   -- ^ 1000, Normal closure; the connection successfully completed
   -- whatever purpose for which it was created.
  | Going_Away
   -- ^ 1001, The endpoint is going away, either because of a server
   -- failure or because the browser is navigating away from the page
   -- that opened the connection.
  | Protocol_Error
   -- ^ 1002, The endpoint is terminating the connection due to a
   -- protocol error.
  | Unsupported_Data
   -- ^ 1003, The connection is being terminated because the endpoint
   -- received data of a type it cannot accept (for example, a
   -- textonly endpoint received binary data).
  | No_Status_Recvd
   -- ^ 1005, Reserved.  Indicates that no status code was provided
   -- even though one was expected.
  | Abnormal_Closure
   -- ^ 1006, Reserved. Used to indicate that a connection was closed
   -- abnormally (that is, with no close frame being sent) when a
   -- status code is expected.
  | Invalid_Frame_Payload_Data
   -- ^ 1007, The endpoint is terminating the connection because a
   -- message was received that contained inconsistent data (e.g.,
   -- nonUTF8 data within a text message).
  | Policy_Violation
   -- ^ 1008, The endpoint is terminating the connection because it
   -- received a message that violates its policy. This is a generic
   -- status code, used when codes 1003 and 1009 are not suitable.
  | Message_Too_Big
   -- ^ 1009, The endpoint is terminating the connection because a
   -- data frame was received that is too large.
  | Missing_Extension
   -- ^ 1010, The client is terminating the connection because it
   -- expected the server to negotiate one or more extension, but the
   -- server didn't.
  | Internal_Error
   -- ^ 1011, The server is terminating the connection because it
   -- encountered an unexpected condition that prevented it from
   -- fulfilling the request.
  | Service_Restart
   -- ^ 1012, The server is terminating the connection because it is
   -- restarting.
  | Try_Again_Later
   -- ^ 1013, The server is terminating the connection due to a
   -- temporary condition, e.g. it is overloaded and is casting off
   -- some of its clients.
  | TLS_Handshake
   -- ^ 1015, Reserved. Indicates that the connection was closed due
   -- to a failure to perform a TLS handshake (e.g., the server
   -- certificate can't be verified).
  | OtherCode Int
   -- ^ OtherCode that is reserved and not in the range 0999
  deriving (Show, Eq, Generic)

instance ToJSVal CloseCode
instance FromJSVal CloseCode

-- | Create a new low-level websocket.
newWebSocket_ :: MisoString -> [MisoString] -> IO (Either MisoString WebSocket_)
{-# INLINE newWebSocket_ #-}
newWebSocket_ url protocols = do
  result <- newWebSocketOrError' url =<< toJSVal protocols
  case isWebSocket' result of
    True -> pure (Right (WebSocket_ result))
    False -> Left <$> getExceptionMessage' result

-- | Create a new high-level websocket.
newWebSocketWithProtocols
  :: MisoString -> [MisoString] -> IO (Either MisoString WebSocket)
{-# INLINE newWebSocketWithProtocols #-}
newWebSocketWithProtocols url protocols = newWebSocket_ url protocols >>= \case
  Left err -> pure (Left err)
  Right socket_ -> Right . WebSocket url protocols <$> newIORef socket_

-- | Create a new high-level websocket with an empty protocol list
newWebSocket :: MisoString -> IO (Either MisoString WebSocket)
{-# INLINE newWebSocket #-}
newWebSocket = flip newWebSocketWithProtocols []

-- | Close a high-level websocket.
closeWebSocket :: WebSocket -> IO ()
closeWebSocket (WebSocket _ _ ref) = readIORef ref >>= closeWebSocket_

-- | Close a websocket with the default code (1000)
closeWebSocket_ :: WebSocket_ -> IO ()
closeWebSocket_ = closeWebSocketWithCode 1000

-- | Default handler for a closed event. Attempts to reconnect if
-- | close was abnormal.
reconnectIfAbnormal :: WebSocketClosedEvent -> IO Bool
reconnectIfAbnormal event = pure (closedCode event /= Normal_Closure)

-- | WebSocket subscription
websocketSub
  :: FromJSON message
  => WebSocket
  -> (WebSocketEvent message -> action) -- ^ Message handler
  -> Sub action model -- ^ A subscription
websocketSub socket handler getModel writeEvent = do
  websocketSubWithCloseHandler socket handler reconnectIfAbnormal
                               getModel writeEvent

-- | WebSocket subscription, given a particular handler for closing events.
websocketSubWithCloseHandler
  :: FromJSON message
  => WebSocket -- ^ Handle to the socket.
  -> (WebSocketEvent message -> action) -- ^ Event handler.
  -> (WebSocketClosedEvent -> IO Bool) -- ^ Decide whether to reconnect on close.
  -> Sub action model
websocketSubWithCloseHandler socket handler handleClose getModel writeEvent = do
  -- Will receive a value when the socket closes.
  closedEventMV <- newEmptyMVar

  -- Spin off the reconnection thread
  void $ forkIO $ do
    reconnect <- takeMVar closedEventMV >>= handleClose
    when reconnect $ do
      let (url, protocols) = (getSocketURL socket, wsProtocols socket)
      socketOrErr <- newWebSocket_ url protocols
      case socketOrErr of
        Right ws -> do
          setSocket_ socket ws
          websocketSubWithCloseHandler socket handler handleClose
                                       getModel writeEvent
        Left err -> putStrLn $ "Error when reconnecting to websocket "
                            <> show socket <> ": " <> show err

  -- Set up event handlers on the socket
  socket_ <- getSocket_ socket
  onOpen socket_ =<< do
    asyncCallback (writeEvent (handler WebSocketOpen))

  onMessage socket_ =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      writeEvent $ handler (WebSocketMessage d)

  onClose socket_ =<< do
    asyncCallback1 $ \e -> do
      closedCode <- codeToCloseCode <$> getCode e
      closedReason <- getReason e
      closedCleanly <- wasClean e
      let closedEvent = WebSocketClosedEvent {..}
      writeEvent $ handler (WebSocketClosed closedEvent)
      putMVar closedEventMV closedEvent

  onError socket_ =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      writeEvent $ handler (WebSocketError d)

-- | Read the IORef in a WebSocket to get its WebSocket_
getSocket_ :: WebSocket -> IO WebSocket_
getSocket_ = readIORef . wsSocketRef

-- | Set the IORef on a WebSocket.
setSocket_ :: WebSocket -> WebSocket_ -> IO ()
setSocket_ ws ws_ = writeIORef (wsSocketRef ws) ws_

-- | Retrieves current status of `socket`
getSocketState :: WebSocket -> IO SocketState
getSocketState socket = toEnum <$> (getSocketState' =<< getSocket_ socket)

-- | Send a JSON-able message to a websocket.
sendJsonToWebSocket :: ToJSON json => WebSocket -> json -> IO ()
sendJsonToWebSocket socket m = sendTextToWebSocket socket =<< stringify m

-- | Send arbitrary text to a websocket.
sendTextToWebSocket :: WebSocket -> MisoString -> IO ()
sendTextToWebSocket socket m = do
  socket_ <- getSocket_ socket
  send' socket_ m

-- | Convert a numeric close code to the enumerated type.
codeToCloseCode :: Int -> CloseCode
codeToCloseCode = \case
  1000 -> Normal_Closure
  1001 -> Going_Away
  1002 -> Protocol_Error
  1003 -> Unsupported_Data
  1005 -> No_Status_Recvd
  1006 -> Abnormal_Closure
  1007 -> Invalid_Frame_Payload_Data
  1008 -> Policy_Violation
  1009 -> Message_Too_Big
  1010 -> Missing_Extension
  1011 -> Internal_Error
  1012 -> Service_Restart
  1013 -> Try_Again_Later
  1015 -> TLS_Handshake
  n    -> OtherCode n


--------------------------------------------------------------------------------
-- * FFI
--------------------------------------------------------------------------------

foreign import javascript unsafe "$1.send($2);"
  send' :: WebSocket_ -> JSString -> IO ()

foreign import javascript unsafe
  "try { $r = new WebSocket($1, $2); } catch (e) { $r = e; }"
  newWebSocketOrError' :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe "$2.close($1);"
  closeWebSocketWithCode :: Int -> WebSocket_ -> IO ()

foreign import javascript unsafe "$r = $1.readyState;"
  getSocketState' :: WebSocket_ -> IO Int

-- | Use this to figure out if websocket creation was successful.
foreign import javascript safe "$r = typeof $1 === 'string';"
  isWebSocket' :: JSVal -> Bool

-- | Get the message off of the exception so that we can return it.
foreign import javascript safe "$r = $1.message;"
  getExceptionMessage' :: JSVal -> IO MisoString

foreign import javascript unsafe "$1.onopen = $2"
  onOpen :: WebSocket_ -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2"
  onClose :: WebSocket_ -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onmessage = $2"
  onMessage :: WebSocket_ -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2"
  onError :: WebSocket_ -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.data"
  getData :: JSVal -> IO JSVal

foreign import javascript unsafe "$r = $1.wasClean"
  wasClean :: JSVal -> IO Bool

foreign import javascript unsafe "$r = $1.code"
  getCode :: JSVal -> IO Int

foreign import javascript unsafe "$r = $1.reason"
  getReason :: JSVal -> IO MisoString

foreign import javascript unsafe "$r = ($1 === $2);"
  webSocketsEqual' :: WebSocket_ -> WebSocket_ -> Bool
