{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription
  ( addSub
  , Sub
  , module Miso.Subscription.Mouse
  , module Miso.Subscription.Keyboard
  , module Miso.Subscription.History
  , module Miso.Subscription.WebSocket
  , module Miso.Subscription.Window
  , module Miso.Subscription.SSE
  ) where

import Data.IORef (readIORef)

import Miso.Subscription.Mouse
import Miso.Subscription.Keyboard
import Miso.Subscription.History
import Miso.Subscription.WebSocket
import Miso.Subscription.Window
import Miso.Subscription.SSE

import Miso.Html.Internal (Sub)
import Miso.Concurrent (EventWriter(writeEvent))
import Miso.Types (RunningApp(..))

-- | Add a subscription to a running app
addSub :: RunningApp action model -> Sub action model -> IO ()
addSub RunningApp {..} sub = sub (readIORef modelRef) (writeEvent eventWriter)
