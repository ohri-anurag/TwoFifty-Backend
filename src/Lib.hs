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
import Prelude hiding (id)
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
          IntroData playerName playerId ->
            handleIntroPhase playerName playerId gameName conn

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
  
  handleIntroPhase :: T.Text -> T.Text -> T.Text -> Connection -> IO ()
  handleIntroPhase playerName playerId gameName conn = do
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
                sendTextData conn $ encode $ ExistingPlayers $ map (fst . fst) players

                -- Append the new player to the existing players
                pure $ IntroState $ players ++ [((playerName, playerId), conn)]

              | length players == 5 -> do
                putStrLn $ "Adding player: " ++ T.unpack playerName
                putStrLn $ "Moving " ++ T.unpack gameName ++ " to bidding round"

                -- Get the cards for each player
                distributedCards <- shuffledCards

                let
                  -- Add the 6th player
                  newPlayers = players ++ [((playerName, playerId), conn)]
                  initPlayerDataSet = fromIntroData $ zip distributedCards newPlayers
                  playerNames = zip playerIndices $ map (fst . fst) newPlayers

                forIndex_ initPlayerDataSet $ \myIndex playerData ->
                  sendTextData (connection playerData)
                    $ encode
                    $ GameData playerNames Player1 myIndex
                    $ cards playerData

                -- Move the state to bidding state
                pure
                  $ BiddingState
                      (CommonStateData Player1 initPlayerDataSet)
                  $ BiddingStateData
                    playerIndices
                    Player1
                    150

              | otherwise -> pure state

            BiddingState commonStateData biddingStateData -> do
              -- There are already 6 players in the game.
              -- Check to see if the player got disconnected and is trying to join again
              let
                didMatchSucceed = not $ null $ matchingPlayers commonStateData
                (player, _) = head $ matchingPlayers commonStateData
              if didMatchSucceed
                then do
                  sendTextData conn
                    $ encode
                    $ BiddingReconnectionData player commonStateData biddingStateData
                  let
                    newCommonStateData = updateConnection player conn commonStateData
                  pure $
                    BiddingState newCommonStateData biddingStateData
                else pure state

            -- RoundState commonStateData roundStateData -> do
            --   -- There are already 6 players in the game.
            --   -- Check to see if the player got disconnected and is trying to join again
            --   let
            --     didMatchSucceed = not $ null $ matchingPlayers commonStateData
            --     (player, _) = head $ matchingPlayers commonStateData
            --   if didMatchSucceed
            --     then do
            --       sendTextData conn
            --         $ encode
            --         $ RoundReconnectionData
            --       let
            --         newCommonStateData = updateConnection player conn commonStateData
            --       pure $
            --         RoundState newCommonStateData roundStateData
            --     else pure state
            _ -> pure state

        -- Game does not exist, create one
        Nothing -> do
          putStrLn $ "Game: " ++ T.unpack gameName ++ " does not exist, Creating..."
          putStrLn $ "Adding player: " ++ T.unpack playerName

          pure $ IntroState [((playerName, playerId), conn)]

    updateState stateMapMVar gameName newState
      where
        matchingPlayers commonStateData = filter ((== playerId) . id . snd) $ toList $ playerDataSet commonStateData
        updateConnection p c commonStateData = commonStateData
          { playerDataSet = updatePlayerData p (\pData -> pData
              { connection = c
              , name = playerName
              }
            ) $ playerDataSet commonStateData
          }
        newPlayerJoined otherPlayerConnections =
          for_ otherPlayerConnections $ \oConn ->
            sendTextData oConn $ encode $ PlayerJoined playerName

  handleBidding :: T.Text -> PlayerIndex -> Int -> IO ()
  handleBidding gameName bidder bidAmount = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          BiddingState commonStateData biddingStateData
            | bidAmount > highestBid biddingStateData && bidAmount <= 250 -> do
                putStrLn $ "Received new highest bid of " ++ show bidAmount ++ ", from " ++ show bidder ++
                  " in " ++ T.unpack gameName
                updateState stateMapMVar gameName
                  $ BiddingState commonStateData
                  $ biddingStateData
                    { highestBid = bidAmount
                    , highestBidder = bidder
                    }
                
                -- Inform the players of the new highest bid
                forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
                  sendTextData (connection playerData) $ encode $ MaximumBid bidder bidAmount

                when (bidAmount == 250) $
                  -- Send quit message for every remaining bidder
                  for_ (bidders biddingStateData) $ \remainingBidder ->
                    forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
                      sendTextData (connection playerData) $ encode $ HasQuitBidding remainingBidder

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
          BiddingState commonStateData biddingStateData -> do
            putStrLn $ show quitter ++ " has decided to quit bidding in " ++ T.unpack gameName
              ++ ", max bid: " ++ show (highestBid biddingStateData)
              ++ ", bidder: " ++ show (highestBidder biddingStateData)

            updateState stateMapMVar gameName
              $ BiddingState commonStateData
              $ biddingStateData
                { bidders = filter ( /= quitter) $ bidders biddingStateData }

            forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
              sendTextData (connection playerData) $ encode $ HasQuitBidding quitter

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
          BiddingState commonStateData biddingStateData -> do
            putStrLn $ show (highestBidder biddingStateData) ++ " has selected trump: "
              ++ show trump
              ++ " and helpers: " ++ show helpers

            let bidTeam = calculateBiddingTeam (highestBidder biddingStateData) commonStateData

            putStrLn $ "Bidding Team: " ++ show bidTeam

            updateState stateMapMVar gameName
              $ RoundState commonStateData
              $ RoundStateData
                  Round1
                  trump
                  bidTeam
                  (firstBidder commonStateData)
                  (firstBidder commonStateData)
                  M.empty
                  (highestBid biddingStateData)

            -- Send the trump and helpers to all players
            forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
              sendTextData (connection playerData) $ encode $ SentSelectionData $ SelectionData trump helpers
          
          _ ->
            pure ()

      Nothing ->
        pure ()
    where
    calculateBiddingTeam bidder commonStateData =
      (:) bidder $ map fst $ filter isPlayerHelper $ toList $ playerDataSet commonStateData
      where
      isPlayerHelper (_, player) =
        let
          playerCards = cards player
        in
        any ( `elem` playerCards) helpers

  handlePlayedCard :: T.Text -> Card -> IO ()
  handlePlayedCard gameName card = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          RoundState commonStateData roundStateData -> do
            putStrLn $ show (currentTurn roundStateData) ++ " has played " ++ show card

            -- Update the hand
            let
              newHand = M.insert (currentTurn roundStateData) card $ hand roundStateData
              newRoundStateData = roundStateData
                { hand = newHand
                , currentTurn = nextTurn $ currentTurn roundStateData
                }
            updateState stateMapMVar gameName
              $ RoundState commonStateData newRoundStateData

            -- When all 6 players have played their turn
            when (M.size newHand == 6) $
              handleRoundFinish commonStateData newRoundStateData

            -- Update all the players that a card has been played
            forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
              sendTextData (connection playerData) $ encode $ PlayCard card
          
          _ ->
            pure ()

      Nothing ->
        pure ()
    where
    handleRoundFinish commonStateData roundStateData =
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
              myCards = M.toList newHand
              wasTrumpUsed = any ( (==) trump . suit . snd) myCards
              comparedSuit = if wasTrumpUsed then trump else base
              winner =
                fst $ maximumBy (compare `on` snd) $ filter ((==) comparedSuit . suit . snd) myCards
              score = sum $ map (calculateScore . snd) myCards
              newCommonStateData = commonStateData
                { playerDataSet = updateGameScore winner score $ playerDataSet commonStateData }

            putStrLn $ show winner ++ " has won " ++ show (roundIndex roundStateData) ++
              " with a score of " ++ show score

            -- Move on to the next Round, and update the next turn
            updateState stateMapMVar gameName
              $ RoundState newCommonStateData
              $ roundStateData
                  { roundIndex = newRound
                  , currentTurn = winner
                  , firstPlayer = winner
                  , hand = M.empty
                  }

            -- Update the players with the round winner and the score
            forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
              sendTextData (connection playerData) $ encode $ RoundData winner score

            -- When 8 rounds have been completed
            when (newRound == Round1) $
              handleGameFinish newCommonStateData roundStateData

          Nothing ->
            pure ()

    handleGameFinish commonStateData roundStateData = do
      -- Delay this thread by two seconds
      threadDelay $ 2 * 1000 * 1000

      -- Calculate winning team
      let
        bidTeam = biddingTeam roundStateData
        antiTeam = filter ( `notElem` bidTeam) playerIndices
        biddingTeamScore = foldrIndex (\i pData s ->
          if i `elem` bidTeam
            then s + gameScore pData
            else s
          ) 0 (playerDataSet commonStateData)
        bidAmount = bid roundStateData
        (winningTeam, winningTeamScore)
          -- Bidding Team won
          | biddingTeamScore >= bidAmount = (bidTeam, bidAmount)
          -- Bidding team lost, but anti team could not score 100
          | biddingTeamScore > 150 = (antiTeam, 250 - biddingTeamScore)
          -- Anti team scored 100+
          | otherwise = (antiTeam, bidAmount)


      putStrLn $ show winningTeam ++ " have won the game with a score of " ++ show winningTeamScore

      forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
        sendTextData (connection playerData) $ encode $ GameFinishedData winningTeam winningTeamScore

      void $ forkIO $ do
        -- Delay this thread by two seconds
        threadDelay $ 2 * 1000 * 1000

        -- Update the state to bidding state, return the scores to zero
        distributedCards <- shuffledCards
        let
          newCommonStateData = foldr
            (\(i, myCards) csd ->
              let myScore = if i `elem` winningTeam then winningTeamScore else 0
              in csd
                { playerDataSet = updateTotalScoreAndCards i (myScore, myCards) $ playerDataSet csd
                }
            )
            (commonStateData { firstBidder = nextTurn $ firstBidder commonStateData })
            $ zip playerIndices distributedCards
        updateState stateMapMVar gameName
          $ BiddingState newCommonStateData
          $ BiddingStateData
            { bidders = playerIndices
            , highestBidder = nextTurn $ firstBidder newCommonStateData
            , highestBid = 150
            }

        -- Send new cards
        forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
          sendTextData (connection playerData)
            $ encode
            $ NewGame $ cards playerData