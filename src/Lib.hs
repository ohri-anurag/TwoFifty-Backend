{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever, void, when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Foldable (for_, maximumBy)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection
import Servant
import Servant.API.WebSocket
import System.Environment (getEnv)


import Bid
import Card
import qualified PlayedCard as PC
import qualified Player as P
import Game --(GameData(..), PlayerIndex(..), PlayerSet, addPlayerAndConnection, initGameData, intToPlayerIndex, listConnections, newPlayer)
import qualified SelectionData as SD
import State


type API = "game" :> WebSocket
        :<|> Raw

startApp :: IO ()
startApp = do
  port <- read <$> getEnv "PORT"
  stateMap <- newMVar M.empty
  run port $ app stateMap

app :: MVar StateMap -> Application
app stateMap = serve api $ server stateMap

api :: Proxy API
api = Proxy

server :: MVar StateMap -> Server API
server stateMapMVar = streamData :<|> serveDirectoryFileServer "public/"
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

              Left _ ->
                case eitherDecode' bytes of
                  Right playedCard ->
                    sendCard playedCard

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
            , biddingPlayers = S.fromList P.playerIndices
            , selectionData = SD.initSelectionData
            , hand = emptyHand
            , biddingTeam = []
            , antiTeam = []
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
            when (isBiddingCompleted newState) $ do
              -- Add the highest bidder to the bidding team
              let newerState = newState { biddingTeam = [oldHighestBidder] }
              updateState stateMapMVar gName newerState

              closeBidding oldHighestBidder oldBid $ gameData newerState

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

        putStrLn $ "Trump Selected: " ++ show (SD.selectedTrump sendSelectionData)
        putStrLn $ "Helper 1: " ++ show (SD.helper1 sendSelectionData)
        putStrLn $ "Helper 2: " ++ show (SD.helper2 sendSelectionData)

        -- Update the state with the received selection data
        updateState stateMapMVar gName
          $ state { selectionData = sendSelectionData }

        for_ connectionList $ \conn ->
          sendTextData conn $ encode sendSelectionData

      Nothing ->
        pure ()

  sendCard :: PC.ReceivePlayedCard -> IO ()
  sendCard (PC.RPC gName card) = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gName stateMap of
      Just state -> do
        -- Send the player and played card to each player
        let
          connectionList = listConnections $ gameData state
          playerTurn = turn $ gameData state
          newHand = updateHand card playerTurn $ hand state
          newTurn = P.nextTurn playerTurn
          newState = state
            { hand = newHand
            , gameData = (gameData state)
                { turn = newTurn }
            }

        putStrLn $ "Player: " ++ P.name (getPlayer playerTurn $ players $ gameData state) ++ " has played card "
                  ++ show card

        -- Update the state with the updated hand, and newTurn
        updateState stateMapMVar gName newState

        for_ connectionList $ \conn ->
          sendTextData conn $ encode $ PC.SPC newTurn card

        -- When next turn comes back to the first bidder, then calculate the score and
        -- send it after an interval of 5 seconds
        when (newTurn == firstBidder (gameData state)) $
          void $ forkIO $ do
            threadDelay $ 5 * 1000 * 1000
            sendRoundScore gName newState newHand

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
  
  sendRoundScore :: String -> State -> Hand -> IO ()
  sendRoundScore gName state fullHand =
    -- First, get the suit of the card played by the first bidder
    case getCardFromHand (firstBidder $ gameData state) fullHand of
      Just (Card _ baseSuit) -> do
        let
          connectionList = listConnections $ gameData state
          cards = getCardsFromHand fullHand
          trump = SD.selectedTrump $ selectionData state

          -- Normally the person with the highest card of `suit` should win
          -- However, if someone used a trump, then the highest trump will win
          wasTrumpUsed = any ( (==) trump . suit . snd) cards
          winner =
            if wasTrumpUsed
              then
                fst $ maximumBy (compare `on` snd) $ filter ((==) trump . suit . snd) cards
              else
                fst $ maximumBy (compare `on` snd) $ filter ((==) baseSuit . suit . snd) cards

          -- Calculate the score
          score = sum $ map (calculateScore . snd) cards
          -- Add score to the player
          newPlayerSet = addScore score winner (players $ gameData state)

        putStrLn $ "Player: " ++ P.name (getPlayer winner $ players $ gameData state) ++ " has won round with score: "
                  ++ show score

        -- Update the state with new scores
        updateState stateMapMVar gName
          $ state
            { gameData = (gameData state)
              { players = newPlayerSet }
            }

        for_ connectionList $ \conn ->
          sendTextData conn $ encode newPlayerSet

      Nothing ->
        pure ()