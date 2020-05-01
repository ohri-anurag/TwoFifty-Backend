{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever, when)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as B
import Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection
import Servant
import Servant.API.WebSocket


import Card

data InitGameState = IGS
  { index :: Int
  , cards :: [Card]
  }

$(deriveJSON defaultOptions ''InitGameState)

type API = "cards" :> Get '[JSON] [Card]
        :<|> "game" :> WebSocket
        :<|> Raw

startApp :: IO ()
startApp = do
  games <- newMVar M.empty
  run 8080 $ app games

app :: MVar (M.Map String [Connection]) -> Application
app games = serve api $ server games

api :: Proxy API
api = Proxy

server :: MVar (M.Map String [Connection]) -> Server API
server gamesMVar = pure allCards :<|> streamData :<|> serveDirectoryFileServer "public/"
  where
  streamData :: (MonadIO m) => Connection -> m ()
  streamData conn = liftIO $ withPingThread conn 10 (pure ()) $ forever $ do
    -- forM_ [1..] $ \i ->
    bytes <- receiveData conn :: IO B.ByteString
    let gameName = T.unpack $ decodeUtf8 bytes
    games <- takeMVar gamesMVar
    newPlayers <- case M.lookup gameName games of
      Just players -> do
        putStrLn $ "Received request, game with name: '" ++ gameName ++ "' already exists. Adding player..."
        pure $ conn : players

      Nothing -> do
        putStrLn $ "Received request, creating game with name: '" ++ gameName ++ "'"
        pure [conn]

    putMVar gamesMVar $ M.insert gameName newPlayers games
    -- sendTextData conn $ encodeUtf8 $ T.pack $ show $ length newPlayers

    when (length newPlayers == 6) $ initiateGame newPlayers

    -- bstr <- receiveData c :: IO B.ByteString
    -- sendTextData c bstr-- >> threadDelay 1000000
    
  initiateGame :: [Connection] -> IO ()
  initiateGame connections = do
    putStrLn "Initializing Game..."
    cards <- shuffledCards
    for_ (zip [1..] $ reverse connections) $ \(i, conn) ->
      sendTextData conn $ encode $ IGS i $ take 8 $ drop (8*(i-1)) cards
