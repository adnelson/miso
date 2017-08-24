{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , lowLevelMiso
  , startApp
  , startLowLevelApp
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
  , module Miso.Types
  , module Miso.Router
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.List
import           Data.Sequence                 ((|>))
import qualified Data.Sequence                 as S
import qualified JavaScript.Object.Internal    as OI
import           JavaScript.Web.AnimationFrame

import           Miso.Concurrent
import           Miso.Delegate
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
import           Miso.Types
import           Miso.FFI

-- | Helper function to abstract out common functionality between `startApp` and `miso`
common
  :: Eq model
  => LowLevelApp model action
  -> model
  -> ((action -> IO ()) -> IO (IORef VTree))
  -> IO ()
common LowLevelApp{..} m getView = do
  -- init Notifier
  notifier@Notify {..} <- newNotify
  -- init empty Model
  modelRef <- newIORef m
  -- init empty actions
  actionsRef <- newIORef S.empty
  let writeEvent a = void . forkIO $ do
        atomicModifyIORef' actionsRef $ \as -> (as |> a, ())
        notify
  -- Create running app object
  let runningApp = RunningApp {
        modelRef = modelRef,
        notifier = notifier,
        recordAppEvent = writeEvent
        }
  -- init Subs
  mapM_ (addSub runningApp) llSubs
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . forkIO . forever $ threadDelay (1000000 * 86400) >> notify
  -- Retrieves reference view
  viewRef <- getView writeEvent
  -- Begin listening for events in the virtual dom
  delegator viewRef llEvents
  -- Process initial action of application
  writeEvent llInitialAction
  -- Program loop, blocking on SkipChan
  forever $ wait >> do
    -- Apply actions to model
    actions <- atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
    (shouldDraw, effects) <- atomicModifyIORef' modelRef $! \oldModel ->
          let (newModel, effects) =
                foldl' (foldEffects writeEvent (llUpdate runningApp))
                  (oldModel, pure ()) actions
          in (newModel, (oldModel /= newModel, effects))
    effects
    when shouldDraw $ do
      newVTree <-
        flip runView writeEvent
          =<< llView <$> readIORef modelRef
      oldVTree <- readIORef viewRef
      void $ waitForAnimationFrame
      Just oldVTree `diff` Just newVTree
      atomicWriteIORef viewRef newVTree

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present.
miso :: (HasURI model, Eq model) => App model action -> IO ()
miso = lowLevelMiso . mkLowLevelApp

-- | Run isomorphic miso application, low-level interface.
lowLevelMiso :: (HasURI model, Eq model) => LowLevelApp model action -> IO ()
lowLevelMiso app@LowLevelApp{..} = do
  uri <- getCurrentURI
  let modelWithUri = setURI uri llModel
  common app llModel $ \writeEvent -> do
    let initialView = llView modelWithUri
    VTree (OI.Object iv) <- flip runView writeEvent initialView
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    newIORef initialVTree

-- | Runs a miso application.
startApp :: Eq model => App model action -> IO ()
startApp = startLowLevelApp . mkLowLevelApp

-- | Runs a miso application, using the low-level app interface.
startLowLevelApp :: Eq model => LowLevelApp model action -> IO ()
startLowLevelApp app@LowLevelApp {..} =
  common app llModel $ \writeEvent -> do
    let initialView = llView llModel
    initialVTree <- flip runView writeEvent initialView
    Nothing `diff` (Just initialVTree)
    newIORef initialVTree

-- | Helper
foldEffects
  :: (action -> IO ())
  -> (action -> model -> Effect action model)
  -> (model, IO ()) -> action -> (model, IO ())
foldEffects sink update = \(!model, !as) action ->
  case update action model of
    Effect newModel effs -> (newModel, newAs)
      where
        newAs = as >> (mapM_ (forkIO . sink =<<) effs)

-- | Translate the higher-level 'App' type into a 'LowLevelApp'
mkLowLevelApp :: App model action -> LowLevelApp model action
mkLowLevelApp app = LowLevelApp {
  llModel = model app,
  llUpdate = \_ -> update app,
  llView = view app,
  llSubs = subs app,
  llEvents = events app,
  llInitialAction = initialAction app
  }
