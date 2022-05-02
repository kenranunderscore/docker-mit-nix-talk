{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.HTTP.Types.Status

-- We're caching numbers, but for simplicity the keys
-- are just strings
type NumberCache = Map String Int

emptyCache :: NumberCache
emptyCache = Map.empty

main :: IO ()
main = do
  let port = 8082
  -- Create a thread-safe caching "location"
  theCache <- newTVarIO emptyCache
  -- The server consists only of interaction with
  -- /cache/:key: reading from the cache and writing
  -- to it
  scotty port $ do
    -- Reading from the cache
    get "/cache/:key" $ do
      k <- param "key"
      cache <- liftIO $ readTVarIO theCache
      case Map.lookup k cache of
        Nothing -> do
          status status404
          json ("Key not found" :: String)
        Just value ->
          json value
    -- Writing to the cache
    put "/cache/:key" $ do
      k <- param "key"
      val :: Int <- jsonData
      -- Update the cache atomically
      liftIO . atomically $
        modifyTVar theCache (Map.alter (const $ Just val) k)
