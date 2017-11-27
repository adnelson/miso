{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.History
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.History
  ( getCurrentURI
  , pushURI
  , replaceURI
  , back
  , forward
  , go
  , uriSub
  , URI (..)
  ) where

import Control.Concurrent     (forkIO)
import Control.Monad          (void, forever)
import GHCJS.Foreign.Callback (Callback, asyncCallback)
import Network.URI            (URI(URI, uriPath))
import System.IO.Unsafe       (unsafePerformIO)

import Miso.Concurrent        (Notify, newNotify, notify, wait)
import Miso.Html.Internal     (Sub)
import Miso.String            (MisoString, pack, unpack)

-- | Add a prefix to a string, unless it already has it.
ensurePrefix :: Char -> String -> String
ensurePrefix p s = case s of
  first:_ | first == p -> s
  _ -> p : s

-- | Retrieves current URI of page
getCurrentURI :: IO URI
{-# INLINE getCurrentURI #-}
getCurrentURI = getURI

-- | Retrieves current URI of page
getURI :: IO URI
{-# INLINE getURI #-}
getURI = do
  URI <$> do unpack <$> getProtocol
      <*> pure Nothing
      <*> do ensurePrefix '/' . unpack <$> getPathName
      <*> do unpack <$> getSearch
      <*> do unpack <$> getHash

-- | Pushes a new URI onto the History stack
pushURI :: URI -> IO ()
{-# INLINE pushURI #-}
pushURI uri = pushStateNoModel uri { uriPath = path }
  where
    path | uriPath uri == mempty = "/"
         | otherwise = ensurePrefix '/' (uriPath uri)

-- | Replaces current URI on stack
replaceURI :: URI -> IO ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = path }
  where
    path | uriPath uri == mempty = "/"
         | otherwise = ensurePrefix '/' (uriPath uri)

-- | Navigates backwards
back :: IO ()
{-# INLINE back #-}
back = back'

-- | Navigates forwards
forward :: IO ()
{-# INLINE forward #-}
forward = forward'

-- | Jumps to a specific position in history
go :: Int -> IO ()
{-# INLINE go #-}
go n = go' n

-- | Notifier for when a URI change occurs
notifier :: Notify
{-# NOINLINE notifier #-}
notifier = unsafePerformIO newNotify

-- | Subscription for `popState` events, from the History API
uriSub :: (URI -> action) -> Sub action model
uriSub = \f _ sink -> do
  void.forkIO.forever $ do
    wait notifier >> do
      sink =<< f <$> getURI
  onPopState =<< do
     asyncCallback $ do
      sink =<< f <$> getURI

foreign import javascript unsafe "window.history.go($1);"
  go' :: Int -> IO ()

foreign import javascript unsafe "window.history.back();"
  back' :: IO ()

foreign import javascript unsafe "window.history.forward();"
  forward' :: IO ()

foreign import javascript unsafe "$r = window.location.pathname;"
  getPathName :: IO MisoString

foreign import javascript unsafe "$r = window.location.search;"
  getSearch :: IO MisoString

foreign import javascript unsafe "$r = window.location.hash;"
  getHash :: IO MisoString

foreign import javascript unsafe "$r = window.location.protocol;"
  getProtocol :: IO MisoString

foreign import javascript unsafe "window.addEventListener('popstate', $1);"
  onPopState :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "window.history.pushState(null, null, $1);"
  pushStateNoModel' :: MisoString -> IO ()

foreign import javascript unsafe "window.history.replaceState(null, null, $1);"
  replaceState' :: MisoString -> IO ()

pushStateNoModel :: URI -> IO ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel u = do
  pushStateNoModel' . pack . show $ u
  notify notifier

replaceTo' :: URI -> IO ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  replaceState' . pack . show $ u
  notify notifier
