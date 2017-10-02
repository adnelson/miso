{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( App (..)

  -- * Application context
  , AppContext (..)
  , newAppContext
  , writeEventTo

  -- * Subscription type
  , Sub
  , addSub

    -- * The Transition Monad
  , Transition
  , fromTransition
  , toTransition
  , scheduleIO
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Monad (void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT)
import           Control.Monad.Trans.Writer.Strict (WriterT(WriterT), Writer, runWriter, tell)
import qualified Data.Map           as M
import           Data.Sequence (Seq, (|>))

import           Miso.Effect
import           Miso.Html.Internal (View)
import           Miso.String
import           Miso.Concurrent (Notify(..), newNotify)

import Data.IORef (IORef, readIORef, newIORef, atomicModifyIORef')

-- | Runtime data for an app.
data AppContext action model = AppContext {
  modelRef :: IORef model,
  -- ^ IO action which returns the current model.
  notifier :: Notify,
  -- ^ Syncronizes app actions and signals new events.
  actionsRef :: IORef (Seq action)
  -- ^ List of actions, for the app to respond to.
  }

-- | Adds an action to the app's event stream.
writeEventTo :: AppContext action model -> action -> IO ()
writeEventTo AppContext {..} action = void . forkIO $ do
  atomicModifyIORef' actionsRef $ \actions -> (actions |> action, ())
  notify notifier

-- | Create a new app context.
newAppContext :: model -> IO (AppContext action model)
newAppContext m = do
  -- init Notifier
  notifier@Notify {..} <- newNotify
  -- init empty Model
  modelRef <- newIORef m
  -- init empty actions
  actionsRef <- newIORef mempty
  pure AppContext {..}

-- | Type synonym for constructing event subscriptions.
--
-- The first argument passed to a subscription provides a way to
-- access the current value of the model (without blocking). The
-- callback is used to dispatch actions which are then fed back to the
-- @update@ function.
type Sub action model = IO model -> (action -> IO ()) -> IO ()

-- type Sub' action model = AppContext action model -> IO ()

-- class Subscription handle where
--   type Event handle :: *
--   makeSub (Event handle -> action) -> Sub' action model

--   subscribe ::

-- | Add a subscription to a running app
addSub :: AppContext action model -> Sub action model -> IO ()
addSub ctx sub = sub (readIORef $ modelRef ctx) (writeEventTo ctx)

-- | Application entry point
data App model action = App
  { model :: model
  -- ^ initial model
  , update :: action -> model -> Effect action model
  -- ^ Function to update model, optionally provide effects.
  --   See the 'Transition' monad for succinctly expressing model transitions.
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action model ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for
  , initialAction :: action
  -- ^ Initial action that is run after the application has loaded
  }

-- | A monad for succinctly expressing model transitions in the 'update' function.
--
-- @Transition@ is a state monad so it abstracts over manually passing the model
-- around. It's also a writer monad where the accumulator is a list of scheduled
-- IO actions. Multiple actions can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library and a single action
-- can be scheduled using 'scheduleIO'.
--
-- Tip: use the @Transition@ monad in combination with the stateful
-- <http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myApp = App
--   { update = 'fromTransition' . \\case
--       MyAction1 -> do
--         field1 .= value1
--         counter += 1
--       MyAction2 -> do
--         field2 %= f
--         scheduleIO $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Transition action model = StateT model (Writer [IO action])

-- | Convert a @Transition@ computation to a function that can be given to 'update'.
fromTransition
    :: Transition action model ()
    -> (model -> Effect action model) -- ^ model 'update' function.
fromTransition act = uncurry Effect . runWriter . execStateT act

-- | Convert an 'update' function to a @Transition@ computation.
toTransition
    :: (model -> Effect action model) -- ^ model 'update' function
    -> Transition action model ()
toTransition f = StateT $ \s ->
                   let Effect s' ios = f s
                   in WriterT $ pure (((), s'), ios)

-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: IO action -> Transition action model ()
scheduleIO ioAction = lift $ tell [ ioAction ]
