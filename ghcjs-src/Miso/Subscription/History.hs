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

import Prelude
import Control.Concurrent
import Control.Monad
import GHCJS.Foreign.Callback
import qualified Data.List    as L
import Miso.Html.Internal     ( Sub )
import Miso.Concurrent
import Miso.String
import Network.URI            hiding (path)
import System.IO.Unsafe

-- | Add a prefix to a string, unless it already has it.
ensurePrefix :: String -> String -> String
ensurePrefix p s = if L.isPrefixOf p s then s else p ++ s

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
      <*> do ensurePrefix "/" . unpack <$> getPathName
      <*> do unpack <$> getSearch
      <*> do unpack <$> getHash

-- | Pushes a new URI onto the History stack
pushURI :: URI -> IO ()
{-# INLINE pushURI #-}
pushURI uri = pushStateNoModel uri { uriPath = path }
  where
    path | uriPath uri == mempty = "/"
         | otherwise = ensurePrefix "/" (uriPath uri)

-- | Replaces current URI on stack
replaceURI :: URI -> IO ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = path }
  where
    path | uriPath uri == mempty = "/"
         | otherwise = ensurePrefix "/" (uriPath uri)

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

chan :: Notify
{-# NOINLINE chan #-}
chan = unsafePerformIO newNotify

-- | Subscription for `popState` events, from the History API
uriSub :: (URI -> action) -> Sub action model
uriSub = \f _ sink -> do
  void.forkIO.forever $ do
    wait chan >> do
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
  notify chan

replaceTo' :: URI -> IO ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  replaceState' . pack . show $ u
  notify chan
