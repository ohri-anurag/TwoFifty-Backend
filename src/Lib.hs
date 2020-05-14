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
import Data.Foldable (for_ , maximumBy)
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection
import Servant
import Servant.API.WebSocket
import System.Environment (getEnv)


import Card
import Player
import SharedData
import State


type API = "game" :> WebSocket
        :<|> Raw

startApp :: IO ()
startApp = do
  port <- read <$> getEnv "PORT"
  -- let port = 8080

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

    case eitherDecode' bytes :: Either String ReceivedData of
      Right (ReceivedData gameName receivedDataValue) ->
        case receivedDataValue of
          IntroData playerName ->
            handleIntroPhase playerName gameName conn

          IncreaseBid bidder bidAmount ->
            handleBidding gameName bidder bidAmount

          QuitBidding quitter ->
            handleQuitting gameName quitter

          ReceivedSelectionData (SelectionData trump helpers) ->
            handleSelectionData gameName trump helpers

          PlayedCard card ->
            handlePlayedCard gameName card

      Left err ->
        putStrLn err
  
  handleIntroPhase :: T.Text -> T.Text -> Connection -> IO ()
  handleIntroPhase playerName gameName conn = do
    stateMap <- readMVar stateMapMVar
    newState <-
      case M.lookup gameName stateMap of
        Just state ->
          case state of
            IntroState players
              | length players < 5 -> do
                putStrLn $ "Adding player: " ++ T.unpack playerName

                -- Inform the existing players that a new player has joined
                newPlayerJoined $ map snd players

                -- Inform the new player of the existing players
                sendTextData conn $ encode $ ExistingPlayers $ map fst players

                -- Append the new player to the existing players
                pure $ IntroState $ players ++ [(playerName, conn)]

              | length players == 5 -> do
                putStrLn $ "Adding player: " ++ T.unpack playerName
                putStrLn $ "Moving " ++ T.unpack gameName ++ " to bidding round"

                -- Get the cards for each player
                distributedCards <- shuffledCards

                let
                  -- Add the 6th player
                  newPlayers = players ++ [(playerName, conn)]
                  connections = zip playerIndices $ map snd newPlayers
                  playerNames = initialisePlayerNameSet $ zip playerIndices $ map fst newPlayers

                for_ connections $ \(myIndex, connection) ->
                  sendTextData connection
                    $ encode
                    $ GameData playerNames Player1 myIndex (getCards myIndex distributedCards)

                -- Move the state to bidding state
                pure
                  $ BiddingState Player1 (M.fromList connections) zeroScores
                  $ BiddingStateData
                    distributedCards
                    playerIndices
                    Player1
                    150

              | otherwise -> pure state

            _ -> pure state


        -- Game does not exist, create one
        Nothing -> do
          putStrLn $ "Game: " ++ T.unpack gameName ++ " does not exist, Creating..."
          putStrLn $ "Adding player: " ++ T.unpack playerName

          pure $ IntroState [(playerName, conn)]

    updateState stateMapMVar gameName newState
      where
        newPlayerJoined otherPlayerConnections =
          for_ otherPlayerConnections $ \connection ->
            sendTextData connection $ encode $ PlayerJoined playerName

  handleBidding :: T.Text -> PlayerIndex -> Int -> IO ()
  handleBidding gameName bidder bidAmount = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          BiddingState firstBidder connectionMap scores biddingStateData
            | bidAmount > highestBid biddingStateData && bidAmount <= 250 -> do
                putStrLn $ "Received new highest bid of " ++ show bidAmount ++ ", from " ++ show bidder ++
                  " in " ++ T.unpack gameName
                updateState stateMapMVar gameName
                  $ BiddingState firstBidder connectionMap scores
                  $ biddingStateData
                    { highestBid = bidAmount
                    , highestBidder = bidder
                    }
                
                -- Inform the players of the new highest bid
                for_ connectionMap $ \conn ->
                  sendTextData conn $ encode $ MaximumBid bidder bidAmount

                when (bidAmount == 250) $
                  -- Send quit message for every remaining bidder
                  for_ (bidders biddingStateData) $ \remainingBidder ->
                    for_ connectionMap $ \conn ->
                      sendTextData conn $ encode $ HasQuitBidding remainingBidder

            | otherwise ->
              pure ()

          _ -> pure ()

      Nothing ->
        pure ()

  handleQuitting :: T.Text -> PlayerIndex -> IO ()
  handleQuitting gameName quitter = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          BiddingState firstBidder connectionMap scores biddingStateData -> do
            putStrLn $ show quitter ++ " has decided to quit bidding in " ++ T.unpack gameName
              ++ ", max bid: " ++ show (highestBid biddingStateData)
              ++ ", bidder: " ++ show (highestBidder biddingStateData)

            updateState stateMapMVar gameName
              $ BiddingState firstBidder connectionMap scores
              $ biddingStateData
                { bidders = filter ( /= quitter) $ bidders biddingStateData }

            for_ connectionMap $ \conn ->
              sendTextData conn $ encode $ HasQuitBidding quitter

          _ ->
            pure ()


      Nothing ->
        pure ()

  handleSelectionData :: T.Text -> Suit -> [Card] -> IO ()
  handleSelectionData gameName trump helpers = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          BiddingState firstBidder connectionMap scores biddingStateData -> do
            putStrLn $ show (highestBidder biddingStateData) ++ " has selected trump: "
              ++ show trump
              ++ " and helpers: " ++ show helpers

            let bidTeam = calculateBiddingTeam (highestBidder biddingStateData) (cardDistribution biddingStateData)

            putStrLn $ "Bidding Team: " ++ show bidTeam

            updateState stateMapMVar gameName
              $ RoundState firstBidder connectionMap scores
              $ RoundStateData
                  Round1
                  trump
                  bidTeam
                  firstBidder
                  firstBidder
                  M.empty
                  (highestBid biddingStateData)

            -- Send the trump and helpers to all players
            for_ connectionMap $ \conn ->
              sendTextData conn $ encode $ SentSelectionData $ SelectionData trump helpers
          
          _ ->
            pure ()

      Nothing ->
        pure ()
    where
    calculateBiddingTeam bidder distributedCards =
      bidder : filter isPlayerHelper playerIndices
      where
      isPlayerHelper player =
        let
          cards = getCards player distributedCards
        in
        any ( `elem` cards) helpers

  handlePlayedCard :: T.Text -> Card -> IO ()
  handlePlayedCard gameName card = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          RoundState firstBidder connectionMap scores roundStateData -> do
            putStrLn $ show (currentTurn roundStateData) ++ " has played " ++ show card

            -- Update the hand
            let
              newHand = M.insert (currentTurn roundStateData) card $ hand roundStateData
              newRoundStateData = roundStateData
                { hand = newHand
                , currentTurn = nextTurn $ currentTurn roundStateData
                }
            updateState stateMapMVar gameName
              $ RoundState firstBidder connectionMap scores newRoundStateData

            -- When all 6 players have played their turn
            when (M.size newHand == 6) $
              handleRoundFinish firstBidder connectionMap scores newRoundStateData

            -- Update all the players that a card has been played
            for_ connectionMap $ \conn ->
              sendTextData conn $ encode $ PlayCard card
          
          _ ->
            pure ()

      Nothing ->
        pure ()
    where
    handleRoundFinish firstBidder connectionMap scores roundStateData =
      void $ forkIO $ do
        let
          newHand = hand roundStateData
          newRound = nextRound $ roundIndex roundStateData
        case M.lookup (firstPlayer roundStateData) newHand of
          Just baseCard -> do
            -- Delay this thread by two seconds
            threadDelay $ 2 * 1000 * 1000

            let
              trump = trumpSuit roundStateData
              base = suit baseCard
              cards = M.toList newHand
              wasTrumpUsed = any ( (==) trump . suit . snd) cards
              comparedSuit = if wasTrumpUsed then trump else base
              winner =
                fst $ maximumBy (compare `on` snd) $ filter ((==) comparedSuit . suit . snd) cards
              score = sum $ map (calculateScore . snd) cards
              newScores = updateScore winner score scores

            putStrLn $ show winner ++ " has won " ++ show (roundIndex roundStateData) ++
              " with a score of " ++ show score

            -- Move on to the next Round, and update the next turn
            updateState stateMapMVar gameName
              $ RoundState firstBidder connectionMap newScores
              $ roundStateData
                  { roundIndex = newRound
                  , currentTurn = winner
                  , firstPlayer = winner
                  , hand = M.empty
                  }

            -- Update the players with the round winner and the score
            for_ connectionMap $ \conn ->
              sendTextData conn $ encode $ RoundData winner score

            -- When 8 rounds have been completed
            when (newRound == Round1) $
              handleGameFinish firstBidder connectionMap newScores roundStateData

          Nothing ->
            pure ()

    handleGameFinish firstBidder connectionMap scores roundStateData = do
      -- Delay this thread by two seconds
      threadDelay $ 2 * 1000 * 1000

      -- Calculate winning team
      let
        bidTeam = biddingTeam roundStateData
        antiTeam = filter ( `notElem` bidTeam) playerIndices
        biddingTeamScore = sum $ map ( `getScore` scores) bidTeam
        bidAmount = bid roundStateData
        (winningTeam, score)
          -- Bidding Team won
          | biddingTeamScore >= bidAmount = (bidTeam, bidAmount)
          -- Bidding team lost, but anti team could not score 100
          | biddingTeamScore > 150 = (antiTeam, 250 - biddingTeamScore)
          -- Anti team scored 100+
          | otherwise = (antiTeam, bidAmount)


      putStrLn $ show winningTeam ++ " have won the game with a score of " ++ show score

      for_ connectionMap $ \conn ->
        sendTextData conn $ encode $ GameFinishedData winningTeam score

      void $ forkIO $ do
        -- Delay this thread by two seconds
        threadDelay $ 2 * 1000 * 1000

        -- Update the state to bidding state, return the scores to zero
        distributedCards <- shuffledCards
        updateState stateMapMVar gameName
          $ BiddingState (nextTurn firstBidder) connectionMap zeroScores
          $ BiddingStateData
            { cardDistribution = distributedCards
            , bidders = playerIndices
            , highestBidder = nextTurn firstBidder
            , highestBid = 150
            }

        -- Send new cards
        let connections = M.toList connectionMap
        for_ connections $ \(myIndex, connection) ->
          sendTextData connection
            $ encode
            $ NewGame $ getCards myIndex distributedCards
