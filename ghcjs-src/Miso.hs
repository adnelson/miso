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
  , startApp
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
  => AppContext action model
  -> App model action
  -> IO (IORef VTree)
  -> IO ()
common (context@AppContext{..}) (App{..}) getView = do
  let writeEvent = writeEventTo context
  -- init Subs
  mapM_ (addSub context) subs
  -- Hack to get around `BlockedIndefinitelyOnMVar` exception
  -- that occurs when no event handlers are present on a template
  -- and `notify` is no longer in scope
  void . forkIO . forever $ threadDelay (1000000 * 86400) >> notify notifier
  -- Retrieves reference view
  viewRef <- getView
  -- Begin listening for events in the virtual dom
  delegator viewRef events
  -- Process initial action of application
  writeEvent initialAction
  -- Program loop, blocking on SkipChan
  forever $ wait notifier >> do
    -- Apply actions to model
    actions <- atomicModifyIORef' actionsRef $ \actions -> (S.empty, actions)
    (shouldDraw, effects) <- atomicModifyIORef' modelRef $! \oldModel ->
          let foldEffects (!oldModel', !as) action = do
                let Effect newModel' effs = update action oldModel'
                    newAs = as >> mapM_ (forkIO . writeEvent =<<) effs
                (newModel', newAs)

              (newModel, effects) =
                foldl' foldEffects (oldModel, pure ()) actions
          in (newModel, (oldModel /= newModel, effects))
    effects
    when shouldDraw $ do
      newVTree <-
        flip runView writeEvent
          =<< view <$> readIORef modelRef
      oldVTree <- readIORef viewRef
      void $ waitForAnimationFrame
      Just oldVTree `diff` Just newVTree
      atomicWriteIORef viewRef newVTree

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present.
miso :: (HasURI model, Eq model) => App model action -> IO ()
miso app@App {..} = do
  uri <- getCurrentURI
  let modelWithUri = setURI uri model
  context <- newAppContext modelWithUri
  common context app $ do
    let initialView = view modelWithUri
    VTree (OI.Object iv) <- flip runView (writeEventTo context) initialView
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree iv
    let initialVTree = VTree (OI.Object iv)
    -- Create virtual dom, perform initial diff
    newIORef initialVTree

-- | Runs a miso application.
startApp :: Eq model => App model action -> IO ()
startApp app@App {..} = do
  context <- newAppContext model
  startAppWithContext context app

-- | Same as @startApp@ but uses a pre-existing context.
startAppWithContext
  :: Eq model => AppContext action model -> App model action -> IO ()
startAppWithContext context (app@App {..})= do
  common context app $ do
    let initialView = view model
    initialVTree <- flip runView (writeEventTo context) initialView
    Nothing `diff` Just initialVTree
    newIORef initialVTree
