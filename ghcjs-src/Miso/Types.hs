-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Types
  ( App (..), RunningApp (..)
  ) where

import qualified Data.Map           as M
import           Data.IORef (IORef)
import           Miso.Effect
import           Miso.Html.Internal
import           Miso.String
import           Miso.Concurrent (EventWriter, Notify)

-- | Runtime data for an app.
data RunningApp action model = RunningApp {
  modelRef :: IORef model,
  notifier :: Notify,
  eventWriter :: EventWriter action
  }

-- | Application entry point
data App model action = App
  { model :: model
  -- ^ initial model
  , update :: RunningApp action model -> action -> model -> Effect action model
  -- ^ Function to update model, optionally provide effects
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action model ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for
  , initialAction :: action
  -- ^ Initial action that is run after the application has loaded
  }
