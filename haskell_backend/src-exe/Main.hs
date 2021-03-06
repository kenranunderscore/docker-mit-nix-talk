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

-- | Keys are just strings here.
type Key = String

-- | We're caching numbers, but for simplicity the keys are just
-- strings
type NumberCache = Map Key Int

emptyCache :: NumberCache
emptyCache = Map.empty

prettyKey :: Key -> String
prettyKey key = "'" <> key <> "'"

getFromCache :: TVar NumberCache -> ActionM ()
getFromCache theCache = do
  k <- param "key"
  liftIO . putStrLn $ "Received request for key " <> prettyKey k
  cache <- liftIO $ readTVarIO theCache
  case Map.lookup k cache of
    Nothing -> do
      liftIO . putStrLn $ "Didn't find " <> prettyKey k <> " in cache"
      status status404
      json ("Key not found" :: String)
    Just value -> do
      liftIO . putStrLn $ "Got it!"
      json value

addToCache :: TVar NumberCache -> ActionM ()
addToCache theCache = do
  k <- param "key"
  val :: Int <- jsonData
  let logMsg =
        mconcat ["Adding ", prettyKey k, " with value ", show val, " to the cache"]
  liftIO $ putStrLn logMsg
  -- Update the cache atomically
  liftIO . atomically $
    modifyTVar theCache (Map.alter (const $ Just val) k)

main :: IO ()
main = do
  let port = 8082
  -- Create a thread-safe caching "location"
  theCache <- newTVarIO emptyCache
  -- The server consists only of interaction with
  -- /cache/:key: reading from the cache and writing
  -- to it
  scotty port $ do
    get "/cache/:key" $ getFromCache theCache
    put "/cache/:key" $ addToCache theCache
