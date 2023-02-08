{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Web.Scotty
import Network.HTTP.Types.Status (status404)
import qualified Network.Wai.Handler.Warp as Warp
import System.Console.ANSI

-- | Keys are just strings here.
type Key = String

-- | We're caching numbers, but for simplicity the keys are just
-- strings.
type NumberCache = Map Key Int

-- | The empty cache.
emptyCache :: NumberCache
emptyCache = Map.empty

-- | Pretty-print a key.
prettyKey :: Key -> String
prettyKey key = "'" <> key <> "'"

logM :: MonadIO m => String -> m ()
logM msg = liftIO $ do
  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Cyan ]
  putStr "[HASKELL] "
  setSGR [ Reset ]
  putStrLn msg

-- | Handle GET /cache/:key by returning a cached value if it exists,
-- or returning a 404 if it doesn't.
getFromCache :: TVar NumberCache -> ActionM ()
getFromCache theCache = do
  k <- param "key"
  logM $ "Received request for key " <> prettyKey k
  cache <- liftIO $ readTVarIO theCache
  case Map.lookup k cache of
    Nothing -> do
      logM $ "Didn't find " <> prettyKey k <> " in cache"
      status status404
      json ("Key not found" :: String)
    Just value -> do
      logM "Got it!"
      json value

-- | Handle PUT /cache/:key by caching the contents of the "key"
-- parameter.
addToCache :: TVar NumberCache -> ActionM ()
addToCache theCache = do
  k <- param "key"
  val :: Int <- jsonData
  let logMsg =
        mconcat ["Adding ", prettyKey k, " with value ", show val, " to the cache"]
  logM logMsg
  -- Update the cache atomically
  liftIO . atomically $
    modifyTVar theCache (Map.alter (const $ Just val) k)

-- | The entry point.  The server is using a fixed port.
main :: IO ()
main = do
  -- Create a thread-safe caching "location".
  theCache <- newTVarIO emptyCache
  -- Server options
  let warpSettings = Warp.setPort 8082 Warp.defaultSettings
      opts = Options { verbose = 0, settings = warpSettings }
  -- The server consists only of interaction with /cache/:key: reading
  -- from the cache and writing to it.
  scottyOpts opts $ do
    get "/cache/:key" $ getFromCache theCache
    put "/cache/:key" $ addToCache theCache
