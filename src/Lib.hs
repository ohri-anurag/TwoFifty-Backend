{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever, when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection
import Servant
import Servant.API.WebSocket


import Bid
import Card
import qualified Player as P
import Game --(GameData(..), PlayerIndex(..), PlayerSet, addPlayerAndConnection, initGameData, intToPlayerIndex, listConnections, newPlayer)
import qualified SelectionData as SD
import State


type API = "cards" :> Get '[JSON] [Card]
        :<|> "game" :> WebSocket
        :<|> Raw

startApp :: IO ()
startApp = do
  stateMap <- newMVar M.empty
  run 8080 $ app stateMap

app :: MVar StateMap -> Application
app stateMap = serve api $ server stateMap

api :: Proxy API
api = Proxy

server :: MVar StateMap -> Server API
server stateMapMVar = pure allCards :<|> streamData :<|> serveDirectoryFileServer "public/"
  where
  streamData :: (MonadIO m) => Connection -> m ()
  streamData conn = liftIO $ withPingThread conn 10 (pure ()) $ forever $ do
    bytes <- receiveData conn :: IO B.ByteString

    case eitherDecode' bytes of
      Right introData -> 
        createNewGame introData conn

      Left _ ->
        case eitherDecode' bytes of
          Right biddingData ->
            handleBidding biddingData

          Left _ ->
            case eitherDecode' bytes of
              Right receiveSelectionData ->
                sendTrump receiveSelectionData

              Left err ->
                putStrLn err

  -- Creates a new game, and handles addition of players to that game
  createNewGame :: P.IntroData -> Connection -> IO ()
  createNewGame introData conn = do
    stateMap <- readMVar stateMapMVar
    let
      gName = P.gameName introData
      player = P.newPlayer $ P.playerName introData

    case M.lookup gName stateMap of
      Just state -> do
        putStrLn
          $ "Received request, game with name: '" ++ gName ++ "' already exists. Adding player: " ++ P.name player
        let
          gData = gameData state
          -- Add player to game data
          newGameData = addPlayerAndConnection player conn gData

        -- Update the MVar with updated state
        updateState stateMapMVar gName 
          $ state { gameData = newGameData }

        -- When all 6 players have joined, initiate the game
        when (nextPlayerIndex newGameData == Just P.Player6) $
          initiateGame gName newGameData

      Nothing -> do
        putStrLn $ "Received request, creating game with name: '" ++ gName ++ "'"
        let newGameData = addPlayerAndConnection player conn initGameData

        -- Update the MVar with newly created state
        updateState stateMapMVar gName
          $ State
            { gameData = newGameData
            , bidData = (P.Player1, 150)
            , biddingPlayers = S.fromList [P.Player1 .. P.Player6]
            , selectionData = SD.initSelectionData
            }

  handleBidding :: ReceiveBiddingData -> IO ()
  handleBidding biddingData = do
    stateMap <- readMVar stateMapMVar
    let
      gName = gameName biddingData
      newBid = bid biddingData
      newPlayerIndex = playerIndex biddingData

    case M.lookup gName stateMap of
      Just state -> do
        let (oldHighestBidder, oldBid) = bidData state

        -- This means that the player has quit bidding
        if newBid == 0
          then do
            putStrLn $ "Player: " ++ show newPlayerIndex ++ " has decided to quit bidding"

            let newState = removePlayerFromBiddingSet newPlayerIndex state

            -- Remove the player from bidding players and update the state
            updateState stateMapMVar gName newState

            -- Tell all players that bidding has been completed if bidding player set is empty
            when (isBiddingCompleted newState) $
              closeBidding oldHighestBidder oldBid $ gameData state
          else if newBid == 250
            then
              closeBidding newPlayerIndex newBid $ gameData state
            else
              when (newBid > oldBid) $ do
                -- Update the state with the new highest bid
                updateState stateMapMVar gName
                  $ state { bidData = (newPlayerIndex, newBid) }

                -- Also inform the players that the max bid has been updated
                updateMaximumBid newPlayerIndex newBid $ gameData state

      Nothing ->
        pure ()

  sendTrump :: SD.ReceiveSelectionData -> IO ()
  sendTrump receiveSelectionData = do
    stateMap <- readMVar stateMapMVar

    let gName = SD.gameName receiveSelectionData
    case M.lookup gName stateMap of
      Just state -> do
        let
          connectionList = listConnections $ gameData state
          sendSelectionData = SD.value receiveSelectionData

        -- Update the state with the received selection data
        updateState stateMapMVar gName
          $ state { selectionData = sendSelectionData }

        for_ connectionList $ \conn ->
          sendTextData conn $ encode sendSelectionData

      Nothing ->
        pure ()


  initiateGame :: String -> GameData -> IO ()
  initiateGame gName gData = do
    putStrLn "Initializing Game..."
    cards <- P.shuffledCards
    let
      connectionList = listConnections gData
    for_ (zip P.playerIndices connectionList) $ \(i, conn) ->
      sendTextData conn $ encode $ P.GS
      { P.playerSet = players gData
      , P.firstBidder = firstBidder gData
      , P.myIndex = i
      , P.myCards = P.getCards i cards
      , P.gameId = gName
      }

  updateMaximumBid :: P.PlayerIndex -> Int -> GameData -> IO ()
  updateMaximumBid newPlayerIndex newBid gData = do
    let connectionList = listConnections gData
    putStrLn $ "Communicating max bid: " ++ show newBid

    for_ connectionList $ \conn ->
      sendTextData conn $ encode $ SBD newPlayerIndex newBid

  closeBidding :: P.PlayerIndex -> Int -> GameData -> IO ()
  closeBidding winner maxBid gData = do
    putStrLn $ "Communcationg Final bid: " ++ show maxBid
    let connectionList = listConnections gData

    for_ connectionList $ \conn ->
      sendTextData conn $ encode $ FBD winner maxBid