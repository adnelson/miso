{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.SSE
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.SSE
 ( -- * Subscription
   sseSub
 , sseSubFromEventSource

   -- * Event source manipulation
 , newEventSource
 , closeEventSource
 , getEventSourceUrl

   -- * Types
 , SSE (..)
 , EventSource
 ) where

import Data.Aeson (FromJSON, Result(..))
import GHCJS.Foreign.Callback (Callback, asyncCallback, asyncCallback1)
import GHCJS.Types (JSVal)
import Miso.FFI (parseResult, jsvalToValue)
import Miso.Html.Internal     ( Sub )
import Miso.String hiding (concat)

-- | Server-sent events Subscription
sseSub :: FromJSON msg => MisoString -> (SSE msg -> action) -> Sub action model
sseSub url eventHandler ioModel sink = do
  es <- newEventSource url
  sseSubFromEventSource es eventHandler ioModel sink

-- | Server-sent events Subscription given an existing EventSource
sseSubFromEventSource ::
  FromJSON msg => EventSource -> (SSE msg -> action) -> Sub action model
sseSubFromEventSource eventSource f _ = \sink -> do
  onMessage eventSource =<< do
    asyncCallback1 $ \jval -> do
      data_ <- getData jval
      parseResult data_ >>= \case
        Success message -> sink $ f (SSEMessage message)
        Error e -> do
          Just val <- jsvalToValue data_
          error $ concat [ "Couldn't parse event data "
                         , show val, ": ", e]
  onError eventSource =<< do
    asyncCallback $
      sink (f SSEError)
  onClose eventSource =<< do
    asyncCallback $
      sink (f SSEClose)

-- | Server-sent events data
data SSE message
  = SSEMessage message
  | SSEClose
  | SSEError
  deriving (Show, Eq)

foreign import javascript unsafe "$r = $1.data;"
  getData :: JSVal -> IO JSVal

-- | Opaque type representing a handle to an event source.
newtype EventSource = EventSource JSVal

foreign import javascript unsafe "$r = new EventSource($1);"
  newEventSource :: MisoString -> IO EventSource

foreign import javascript unsafe "$1.onmessage = $2;"
  onMessage :: EventSource -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.onerror = $2;"
  onError :: EventSource -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onclose = $2;"
  onClose :: EventSource -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.close();"
  closeEventSource :: EventSource -> IO ()

foreign import javascript unsafe "$r = $1.url;"
  getEventSourceUrl :: EventSource -> IO MisoString

-- | Test URL
-- http://sapid.sourceforge.net/ssetest/webkit.events.php
-- var source = new EventSource("demo_sse.php");
