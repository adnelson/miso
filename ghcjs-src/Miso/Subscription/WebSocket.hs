{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , websocketSubFromWebSocket

    -- * Interacting with the websocket
  , sendJsonToWebSocket
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
import GHC.Generics
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types
import Prelude                 hiding (map)

import Miso.FFI                (parse, stringify)
import Miso.Html.Internal      (Sub)
import Miso.String

-- | WebSocket data type
newtype WebSocket = WebSocket JSVal

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
  = CLOSE_NORMAL
   -- ^ 1000, Normal closure; the connection successfully completed whatever purpose for which it was created.
  | CLOSE_GOING_AWAY
   -- ^ 1001, The endpoint is going away, either because of a server failure or because the browser is navigating away from the page that opened the connection.
  | CLOSE_PROTOCOL_ERROR
   -- ^ 1002, The endpoint is terminating the connection due to a protocol error.
  | CLOSE_UNSUPPORTED
   -- ^ 1003, The connection is being terminated because the endpoint received data of a type it cannot accept (for example, a textonly endpoint received binary data).
  | CLOSE_NO_STATUS
   -- ^ 1005, Reserved.  Indicates that no status code was provided even though one was expected.
  | CLOSE_ABNORMAL
   -- ^ 1006, Reserved. Used to indicate that a connection was closed abnormally (that is, with no close frame being sent) when a status code is expected.
  | Unsupported_Data
   -- ^ 1007, The endpoint is terminating the connection because a message was received that contained inconsistent data (e.g., nonUTF8 data within a text message).
  | Policy_Violation
   -- ^ 1008, The endpoint is terminating the connection because it received a message that violates its policy. This is a generic status code, used when codes 1003 and 1009 are not suitable.
  | CLOSE_TOO_LARGE
   -- ^ 1009, The endpoint is terminating the connection because a data frame was received that is too large.
  | Missing_Extension
   -- ^ 1010, The client is terminating the connection because it expected the server to negotiate one or more extension, but the server didn't.
  | Internal_Error
   -- ^ 1011, The server is terminating the connection because it encountered an unexpected condition that prevented it from fulfilling the request.
  | Service_Restart
   -- ^ 1012, The server is terminating the connection because it is restarting.
  | Try_Again_Later
   -- ^ 1013, The server is terminating the connection due to a temporary condition, e.g. it is overloaded and is casting off some of its clients.
  | TLS_Handshake
   -- ^ 1015, Reserved. Indicates that the connection was closed due to a failure to perform a TLS handshake (e.g., the server certificate can't be verified).
  | OtherCode Int
   -- ^ OtherCode that is reserved and not in the range 0999
  deriving (Show, Eq, Generic)

instance ToJSVal CloseCode
instance FromJSVal CloseCode

-- | Create a new websocket.
createWebSocket :: MisoString -> IO WebSocket
{-# INLINE createWebSocket #-}
createWebSocket url' = createWebSocket' url' =<< toJSVal ("" :: String)

-- | Close a websocket with the default code (1000)
closeWebSocket :: WebSocket -> IO ()
closeWebSocket = closeWebSocketWithCode 1000

-- | Default handler for a closed event. Attempts to reconnect if
-- | close was abnormal.
reconnectIfAbnormal :: WebSocketClosedEvent -> IO Bool
reconnectIfAbnormal event = pure (closedCode event /= CLOSE_NORMAL)

-- | WebSocket subscription
websocketSub
  :: FromJSON message
  => MisoString -- ^ Subscription URI
  -> (WebSocketEvent message -> action) -- ^ Message handler
  -> Sub action model -- ^ A subscription
websocketSub uri handler getModel writeEvent = do
  socket <- createWebSocket uri
  websocketSubFromWebSocket socket handler reconnectIfAbnormal getModel writeEvent

-- | WebSocket subscription, given a particular websocket.
websocketSubFromWebSocket
  :: FromJSON message
  => WebSocket -- ^ Handle to the socket.
  -> (WebSocketEvent message -> action) -- ^ Event handler.
  -> (WebSocketClosedEvent -> IO Bool) -- ^ Decide whether to reconnect on close.
  -> Sub action model
websocketSubFromWebSocket socket handler shouldReconnect getModel writeEvent = do
  -- Will receive a value when the socket closes.
  closedEventMV <- newEmptyMVar

  -- Spin off the reconnection thread
  void $ forkIO $ do
    takeMVar closedEventMV >>= shouldReconnect >>= \case
      False -> pure () -- Don't reconnect, just return.
      True -> do
        newSocket <- createWebSocket (getSocketURL socket)
        websocketSubFromWebSocket newSocket handler shouldReconnect
                                  getModel writeEvent

  -- Set up event handlers on the socket
  onOpen socket =<< do
    asyncCallback (writeEvent (handler WebSocketOpen))

  onMessage socket =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      writeEvent $ handler (WebSocketMessage d)

  onClose socket =<< do
    asyncCallback1 $ \e -> do
      closedCode <- codeToCloseCode <$> getCode e
      closedReason <- getReason e
      closedCleanly <- wasClean e
      let closedEvent = WebSocketClosedEvent {..}
      writeEvent $ handler (WebSocketClosed closedEvent)
      putMVar closedEventMV closedEvent

  onError socket =<< do
    asyncCallback1 $ \v -> do
      d <- parse =<< getData v
      writeEvent $ handler (WebSocketError d)

-- | Retrieves current status of `socket`
getSocketState :: WebSocket -> IO SocketState
getSocketState socket = toEnum <$> getSocketState' socket

-- | Send a JSON-able message to a websocket.
sendJsonToWebSocket :: ToJSON json => WebSocket -> json -> IO ()
sendJsonToWebSocket socket m = send' socket =<< stringify m

-- | Convert a numeric close code to the enumerated type.
codeToCloseCode :: Int -> CloseCode
codeToCloseCode = \case
  1000 -> CLOSE_NORMAL
  1001 -> CLOSE_GOING_AWAY
  1002 -> CLOSE_PROTOCOL_ERROR
  1003 -> CLOSE_UNSUPPORTED
  1005 -> CLOSE_NO_STATUS
  1006 -> CLOSE_ABNORMAL
  1007 -> Unsupported_Data
  1008 -> Policy_Violation
  1009 -> CLOSE_TOO_LARGE
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
  send' :: WebSocket -> JSString -> IO ()

foreign import javascript unsafe "$r = new WebSocket($1, $2);"
  createWebSocket' :: JSString -> JSVal -> IO WebSocket

foreign import javascript unsafe "$2.close($1);"
  closeWebSocketWithCode :: Int -> WebSocket -> IO ()

foreign import javascript unsafe "$r = $1.readyState;"
  getSocketState' :: WebSocket -> IO Int

foreign import javascript unsafe "$r = $1.url;"
  getSocketURL :: WebSocket -> MisoString

foreign import javascript unsafe "$1.onopen = $2"
  onOpen :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2"
  onClose :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onmessage = $2"
  onMessage :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2"
  onError :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.data"
  getData :: JSVal -> IO JSVal

foreign import javascript unsafe "$r = $1.wasClean"
  wasClean :: JSVal -> IO Bool

foreign import javascript unsafe "$r = $1.code"
  getCode :: JSVal -> IO Int

foreign import javascript unsafe "$r = $1.reason"
  getReason :: JSVal -> IO MisoString
