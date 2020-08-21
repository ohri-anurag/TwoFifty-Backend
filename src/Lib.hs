{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Exception (SomeException, handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void, when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (intercalate, nub)
import Data.Foldable (for_ , maximumBy)
import Data.Function (on)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection
import Network.WebSockets (ConnectionException(..))
import Prelude hiding (id)
import Servant hiding (POST)
import Servant.API.WebSocket
import System.Environment (getEnv)


import Card
import Player
import SharedData
import State


type API = "game" :> WebSocket
        :<|> Raw

ioErrorHandler :: IOError -> IO String
ioErrorHandler e = do
  print e
  pure "8080"

failSilentlyHandler :: SomeException -> IO ()
failSilentlyHandler e = do
  putStrLn $ "Encountered error: " ++ show e
  putStrLn "Failing silently"

startApp :: IO ()
startApp = do
  appPort <- read <$> handle ioErrorHandler (getEnv "PORT")

  stateMap <- newMVar M.empty
  run appPort $ app stateMap

app :: MVar StateMap -> Application
app stateMap = serve api $ server stateMap

api :: Proxy API
api = Proxy

connectionExceptionHandler :: ConnectionException -> IO ()
connectionExceptionHandler (CloseRequest _ _) = putStrLn "Client closed the connection"
connectionExceptionHandler ConnectionClosed = putStrLn "Connection closed unexpectedly"
connectionExceptionHandler _ = putStrLn "Connection closed, reason unknown"

sendTextDataSafe :: Connection -> B.ByteString -> IO ()
sendTextDataSafe conn bytes = handle connectionExceptionHandler $ sendTextData conn bytes

server :: MVar StateMap -> Server API
server stateMapMVar = streamData :<|> serveDirectoryFileServer "public/"
  where
  streamData :: (MonadIO m) => Connection -> m ()
  streamData conn = liftIO $ withPingThread conn 10 (pure ()) $ readFromConnection conn

  readFromConnection :: Connection -> IO ()
  readFromConnection conn =
    handle connectionExceptionHandler $ do
      bytes <- receiveData conn :: IO B.ByteString

      case eitherDecode' bytes :: Either String ReceivedData of
        Right (ReceivedData gameName receivedDataValue) ->
          case receivedDataValue of
            IntroData playerName playerId ->
              handleIntroPhase playerName playerId gameName conn

            IncreaseBid bidderIndex bidAmount ->
              handleBidding gameName bidderIndex bidAmount

            QuitBidding quitter ->
              handleQuitting gameName quitter

            ReceivedSelectionData (SelectionData trump helpers) ->
              handleSelectionData gameName trump helpers

            PlayedCard playedCard ->
              handlePlayedCard gameName playedCard

        Left err ->
          putStrLn $ "Received unknown message: " ++ err

      -- If no exception was raised, connection is alright, go and read again
      readFromConnection conn

  handleIntroPhase :: T.Text -> T.Text -> T.Text -> Connection -> IO ()
  handleIntroPhase playerName playerId gameName conn = do
    stateMap <- readMVar stateMapMVar
    newState <-
      case M.lookup gameName stateMap of
        Just state ->
          case state of
            IntroState players
              | length players < 5 ->
                checkForExistingPlayers players state $ do

                  putStrLn $ "Adding player: " ++ T.unpack playerName

                  -- Inform the existing players that a new player has joined
                  newPlayerJoined $ map snd players

                  -- Inform the new player of the existing players
                  putStrLn "Send existing player data to new player"
                  sendTextDataSafe conn $ encode $ ExistingPlayers $ map (fst . fst) players

                  -- Append the new player to the existing players
                  pure $ IntroState $ players ++ [((playerName, playerId), conn)]

              | length players == 5 ->
                checkForExistingPlayers players state $ do

                  putStrLn $ "Adding player: " ++ T.unpack playerName

                  -- Inform the new player of the existing players
                  sendTextDataSafe conn $ encode $ ExistingPlayers $ map (fst . fst) players

                  putStrLn $ "Moving " ++ T.unpack gameName ++ " to bidding round"

                  -- Get the cards for each player
                  distributedCards <- shuffledCards

                  let
                    -- Add the 6th player
                    newPlayers = players ++ [((playerName, playerId), conn)]
                    initPlayerDataSet = fromIntroData $ zip distributedCards newPlayers
                    playerNames = zip playerIndices $ map (fst . fst) newPlayers

                  forIndex_ initPlayerDataSet $ \myIndex playerData ->
                    sendTextDataSafe (connection playerData)
                      $ encode
                      $ GameData playerNames Player1 myIndex
                      $ currentCards playerData

                  -- Move the state to bidding state
                  pure
                    $ BiddingState
                        (CommonStateData Player1 initPlayerDataSet Player1 150)
                        playerIndices

              | otherwise -> pure state

            BiddingState commonStateData bidders -> do
              -- There are already 6 players in the game.
              -- Check to see if the player got disconnected and is trying to join again
              let
                didMatchSucceed = not $ null $ matchingPlayers commonStateData
                (player, _) = head $ matchingPlayers commonStateData
              if didMatchSucceed
                then do
                  sendTextDataSafe conn
                    $ encode
                    $ BiddingReconnectionData player commonStateData bidders
                  let
                    newCommonStateData = updateConnection player conn commonStateData
                  pure $
                    BiddingState newCommonStateData bidders
                else pure state

            RoundState commonStateData roundStateData -> do
              -- There are already 6 players in the game.
              -- Check to see if the player got disconnected and is trying to join again
              let
                didMatchSucceed = not $ null $ matchingPlayers commonStateData
                (player, _) = head $ matchingPlayers commonStateData
              if didMatchSucceed
                then do
                  -- Before sending this, we need to recalibrate every player's status
                  -- according to this player
                  sendTextDataSafe conn
                    $ encode
                    $ RoundReconnectionData
                      player
                      (commonStateData
                        { playerDataSet =
                          recalibrate
                            player
                            (helperCards roundStateData)
                            (biddingTeam roundStateData)
                          $ playerDataSet commonStateData
                        }
                      )
                      roundStateData
                  let
                    newCommonStateData = updateConnection player conn commonStateData
                  pure $
                    RoundState newCommonStateData roundStateData
                else pure state

        -- Game does not exist, create one
        Nothing -> do
          putStrLn $ "Game: " ++ T.unpack gameName ++ " does not exist, Creating..."
          putStrLn $ "Adding player: " ++ T.unpack playerName

          -- Inform the player that there are no existing players
          sendTextDataSafe conn $ encode $ ExistingPlayers []

          pure $ IntroState [((playerName, playerId), conn)]

    updateState stateMapMVar gameName newState
      where
        checkForExistingPlayers players state action
          -- Check if player with same id already exists. If so, reject joining request
          | any ((==) playerId . snd . fst) players = do
              sendTextDataSafe conn $ encode PlayerWithIdAlreadyExists
              pure state

          -- Check if player with same name already exists. If so, reject joining request
          | any ((==) playerName . fst . fst) players = do
              sendTextDataSafe conn $ encode PlayerWithNameAlreadyExists
              pure state

          | otherwise = action
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
            sendTextDataSafe oConn $ encode $ PlayerJoined playerName
        recalibrate player helpers bidTeam playerSet =
          let
            newBidTeam =
              if any (\h -> h `elem` initialCards (getPlayer player playerSet)) helpers
                then player : bidTeam
                else bidTeam
            hasTeamBeenRevealed = all (\h ->
              any (\b -> elem h $ initialCards $ getPlayer b playerSet) newBidTeam
              ) helpers
          in
            foldr (\i pds ->
              let
                newStatus
                  | i `elem` newBidTeam = BiddingTeam
                  | hasTeamBeenRevealed || i == player = AntiTeam
                  | otherwise = Undecided
              in
                updateStatus i newStatus pds
              ) playerSet playerIndices

  handleBidding :: T.Text -> PlayerIndex -> Int -> IO ()
  handleBidding gameName bidderIndex bidAmount = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          BiddingState commonStateData bidders
            | bidAmount > bid commonStateData && bidAmount <= 250 -> do
                putStrLn $ "Received new highest bid of " ++ show bidAmount ++ ", from " ++ show bidderIndex ++
                  " in " ++ T.unpack gameName

                let
                  newCommonStateData = commonStateData
                    { bid = bidAmount
                    , bidder = bidderIndex
                    }

                updateState stateMapMVar gameName
                  $ BiddingState newCommonStateData bidders

                -- Inform the players of the new highest bid
                forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
                  sendTextDataSafe (connection playerData) $ encode $ MaximumBid bidderIndex bidAmount

                when (bidAmount == 250) $
                  -- Send quit message for every remaining bidder
                  for_ bidders $ \remainingBidder ->
                    forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
                      sendTextDataSafe (connection playerData) $ encode $ HasQuitBidding remainingBidder

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
          BiddingState commonStateData bidders -> do
            putStrLn $ show quitter ++ " has decided to quit bidding in " ++ T.unpack gameName
              ++ ", max bid: " ++ show (bid commonStateData)
              ++ ", bidder: " ++ show (bidder commonStateData)

            updateState stateMapMVar gameName
              $ BiddingState commonStateData
              $ filter ( /= quitter) bidders

            forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
              sendTextDataSafe (connection playerData) $ encode $ HasQuitBidding quitter

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
          BiddingState commonStateData _ -> do
            putStrLn $ show (bidder commonStateData) ++ " has selected trump: "
              ++ show trump
              ++ " and helpers: " ++ show helpers

            updateState stateMapMVar gameName
              $ RoundState commonStateData
              $ RoundStateData
                  Round1
                  trump
                  helpers
                  [bidder commonStateData]
                  (firstBidder commonStateData)
                  (firstBidder commonStateData)

            -- Send the trump and helpers to all players
            forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
              sendTextDataSafe (connection playerData) $ encode $ SentSelectionData $ SelectionData trump helpers
          
          _ ->
            pure ()

      Nothing ->
        pure ()

  handlePlayedCard :: T.Text -> Card -> IO ()
  handlePlayedCard gameName playedCard = do
    stateMap <- readMVar stateMapMVar

    case M.lookup gameName stateMap of
      Just state ->
        case state of
          RoundState commonStateData roundStateData -> do
            putStrLn $ show (currentTurn roundStateData) ++ " has played " ++ show playedCard

            -- Update the hand
            let
              newCommonStateData = commonStateData
                { playerDataSet = 
                    updateCard (currentTurn roundStateData) (Just playedCard)
                      $ playerDataSet commonStateData
                }
              newTurn = nextTurn $ currentTurn roundStateData
              newRoundStateData = roundStateData
                { currentTurn = newTurn
                , biddingTeam =
                    if playedCard `elem` helperCards roundStateData
                      then currentTurn roundStateData : biddingTeam roundStateData
                      else biddingTeam roundStateData
                }
              

            updateState stateMapMVar gameName
              $ RoundState newCommonStateData newRoundStateData

            -- When all 6 players have played their turn
            when (newTurn == firstPlayer newRoundStateData) $
              handleRoundFinish newCommonStateData newRoundStateData

            -- Update all the players that a card has been played
            forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
              sendTextDataSafe (connection playerData) $ encode $ PlayCard playedCard
          
          _ ->
            pure ()

      Nothing ->
        pure ()
    where
    handleRoundFinish commonStateData roundStateData =
      void $ forkIO $ do
        let
          newRound = nextRound $ roundIndex roundStateData
        case card (getPlayer (firstPlayer roundStateData) $ playerDataSet commonStateData) of
          Just baseCard -> do
            -- Delay this thread by two seconds
            threadDelay $ 2 * 1000 * 1000

            let
              trump = trumpSuit roundStateData
              base = suit baseCard
              myCards = mapMaybe (\(i, pData) -> (,) i <$> card pData) $ toList $ playerDataSet commonStateData
              wasTrumpUsed = any ( (==) trump . suit . snd) myCards
              comparedSuit = if wasTrumpUsed then trump else base
              winner =
                fst $ maximumBy (compare `on` snd) $ filter ((==) comparedSuit . suit . snd) myCards
              score = sum $ map (calculateScore . snd) myCards
              newPlayerDataSet = updateGameScore winner score $ playerDataSet commonStateData
              newCommonStateData = commonStateData
                { playerDataSet =
                    foldr
                      (\i csd -> updateCard i Nothing csd)
                      newPlayerDataSet
                      playerIndices
                }
              newRoundStateData = roundStateData
                { roundIndex = newRound
                , currentTurn = winner
                , firstPlayer = winner
                }

            putStrLn $ show winner ++ " has won " ++ show (roundIndex newRoundStateData) ++
              " with a score of " ++ show score

            -- Move on to the next Round, and update the next turn
            updateState stateMapMVar gameName
              $ RoundState newCommonStateData newRoundStateData

            -- Update the players with the round winner and the score
            forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
              sendTextDataSafe (connection playerData) $ encode $ RoundData winner score

            -- When 8 rounds have been completed
            when (newRound == Round1) $
              handleGameFinish newCommonStateData newRoundStateData

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
        bidAmount = bid commonStateData
        (winningTeam, winningTeamScore)
          -- Bidding Team won
          | biddingTeamScore >= bidAmount = (bidTeam, bidAmount)
          -- Bidding team lost, but anti team could not score 100
          | biddingTeamScore > 150 = (antiTeam, 250 - biddingTeamScore)
          -- Anti team scored 100+
          | otherwise = (antiTeam, bidAmount)


      putStrLn $ show winningTeam ++ " have won the game with a score of " ++ show winningTeamScore

      -- Send update to DB to persist the data
      let
        playerString playerIndex =
          let
            player = getPlayer playerIndex $ playerDataSet commonStateData
          in
            intercalate ":" $ map T.unpack [id player, name player]

        gameString = intercalate ";"
          [ T.unpack gameName
          , intercalate "," $ map show [bidAmount, biddingTeamScore]
          , intercalate "|"
            [ show $ trumpSuit roundStateData
            , intercalate ","
              $ map (\(Card v s) ->
                intercalate ":" [show v, show s]
                )
              $ helperCards roundStateData
            ]
          , intercalate "|"
            [ intercalate "," $ map playerString $ reverse $ nub bidTeam
            , intercalate "," $ map playerString antiTeam
            ]
          ]

      putStrLn gameString

      handle failSilentlyHandler $ void $ runReq defaultHttpConfig $ req
        POST
        (https "two-fifty-analytics.herokuapp.com")
        (ReqBodyBs $ encodeUtf8 $ T.pack gameString)
        ignoreResponse
        mempty

      forIndex_ (playerDataSet commonStateData) $ \_ playerData ->
        sendTextDataSafe (connection playerData) $ encode $ GameFinishedData winningTeam winningTeamScore

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
            ( commonStateData
              { firstBidder = nextTurn $ firstBidder commonStateData
              , bidder = nextTurn $ firstBidder commonStateData
              , bid = 150
              }
            )
            $ zip playerIndices distributedCards

        updateState stateMapMVar gameName
          $ BiddingState newCommonStateData playerIndices

        -- Send new cards
        forIndex_ (playerDataSet newCommonStateData) $ \_ playerData ->
          sendTextDataSafe (connection playerData)
            $ encode
            $ NewGame $ currentCards playerData