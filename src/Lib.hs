{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    , app
    ) where

-- import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever) --, void, when)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Foldable ({-foldl', -}for_) -- , maximumBy)
-- import Data.Function (on)
import qualified Data.Map as M
-- import qualified Data.Set as S
import qualified Data.Text as T
-- import Data.Text.Encoding (encodeUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection
import Servant
import Servant.API.WebSocket
-- import System.Environment (getEnv)


-- import Bid
-- import Card
-- import qualified PlayedCard as PC
-- import qualified Player as P
-- import Game --(GameData(..), PlayerIndex(..), PlayerSet, addPlayerAndConnection, initGameData, intToPlayerIndex, listConnections, newPlayer)
-- import qualified SelectionData as SD
import SharedData
import Player
import State


type API = "game" :> WebSocket
        :<|> Raw

startApp :: IO ()
startApp = do
  -- port <- read <$> getEnv "PORT"
  let port = 8080

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
          _ ->
            putStrLn "Oye hoye ni kudiyan sheher diyan"
      Left err ->
        putStrLn err
  
  handleIntroPhase :: T.Text -> T.Text -> Connection -> IO ()
  handleIntroPhase playerName gameName conn = do
    putStrLn $ T.unpack playerName ++ " , " ++ T.unpack gameName

    stateMap <- readMVar stateMapMVar
    newState <-
      case M.lookup gameName stateMap of
        Just state ->
          case state of
            IntroState players
              | length players < 5 -> do
                -- Inform the existing players that a new player has joined
                newPlayerJoined $ map snd players

                -- Inform the new player of the existing players
                sendTextData conn $ encode $ ExistingPlayers $ map fst players

                -- Append the new player to the existing players
                pure $ IntroState $ players ++ [(playerName, conn)]

              | length players == 5 -> do
                -- Get the cards for each player
                cardDistribution <- shuffledCards

                let
                  -- Add the 6th player
                  newPlayers = players ++ [(playerName, conn)]
                  connections = zip playerIndices $ map snd newPlayers
                  playerNames = initialisePlayerNameSet $ zip playerIndices $ map fst newPlayers

                for_ connections $ \(myIndex, connection) ->
                  sendTextData connection
                    $ encode
                    $ GameData playerNames Player1 myIndex (getCards myIndex cardDistribution)

                -- Move the state to bidding state
                pure $
                  BiddingState
                    playerNames
                    cardDistribution
                    (M.fromList connections)
                    Player1
                    Player1
                    150

              | otherwise -> pure state

            _ -> pure state


        -- Game does not exist, create one
        Nothing ->
          pure $ IntroState [(playerName, conn)]

    updateState stateMapMVar gameName newState
      where
        newPlayerJoined otherPlayerConnections =
          for_ otherPlayerConnections $ \connection ->
            sendTextData connection $ encode $ PlayerJoined playerName

    -- case eitherDecode' bytes of
    --   Right introData -> 
    --     createNewGame introData conn

    --   Left _ ->
    --     case eitherDecode' bytes of
    --       Right biddingData ->
    --         handleBidding biddingData

    --       Left _ ->
    --         case eitherDecode' bytes of
    --           Right receiveSelectionData ->
    --             sendTrump receiveSelectionData

    --           Left _ ->
    --             case eitherDecode' bytes of
    --               Right playedCard ->
    --                 sendCard playedCard

    --               Left err ->
    --                 putStrLn err

  -- Creates a new game, and handles addition of players to that game
  -- createNewGame :: P.IntroData -> Connection -> IO ()
  -- createNewGame introData conn = do
  --   stateMap <- readMVar stateMapMVar
  --   let
  --     gName = P.gameName introData
  --     player = P.newPlayer $ P.playerName introData

  --   case M.lookup gName stateMap of
  --     Just state -> do
  --       putStrLn
  --         $ "Received request, game with name: '" ++ gName ++ "' already exists. Adding player: " ++ P.name player
  --       let
  --         gData = gameData state
  --         -- Add player to game data
  --         newGameData = addPlayerAndConnection player conn gData

  --       -- Update the MVar with updated state
  --       updateState stateMapMVar gName 
  --         $ state { gameData = newGameData }

  --       -- When all 6 players have joined, initiate the game
  --       when (nextPlayerIndex newGameData == Just P.Player6) $
  --         initiateGame gName newGameData

  --     Nothing -> do
  --       putStrLn $ "Received request, creating game with name: '" ++ gName ++ "'"
  --       let newGameData = addPlayerAndConnection player conn initGameData

  --       -- Update the MVar with newly created state
  --       updateState stateMapMVar gName
  --         $ State
  --           { gameData = newGameData
  --           , bidData = (P.Player1, 150)
  --           , biddingPlayers = S.fromList P.playerIndices
  --           , selectionData = SD.initSelectionData
  --           , firstPlayer = firstBidder newGameData
  --           , hand = emptyHand
  --           , biddingTeam = []
  --           , antiTeam = []
  --           , roundIndex = Round1
  --           , helpersRevealed = 0
  --           }

  -- handleBidding :: ReceiveBiddingData -> IO ()
  -- handleBidding biddingData = do
  --   stateMap <- readMVar stateMapMVar
  --   let
  --     gName = gameName biddingData
  --     newBid = bid biddingData
  --     newPlayerIndex = playerIndex biddingData

  --   case M.lookup gName stateMap of
  --     Just state -> do
  --       let (oldHighestBidder, oldBid) = bidData state

  --       -- This means that the player has quit bidding
  --       if newBid == 0
  --         then do
  --           putStrLn $ "Player: " ++ show newPlayerIndex ++ " has decided to quit bidding"

  --           let newState = removePlayerFromBiddingSet newPlayerIndex state

  --           -- Remove the player from bidding players and update the state
  --           updateState stateMapMVar gName newState

  --           -- Tell all players that bidding has been completed if bidding player set is empty
  --           when (isBiddingCompleted newState) $ do
  --             -- Add the highest bidder to the bidding team
  --             let newerState = newState { biddingTeam = [oldHighestBidder] }
  --             updateState stateMapMVar gName newerState

  --             closeBidding oldHighestBidder oldBid $ gameData newerState

  --         else if newBid == 250
  --           then do
  --             -- Add the newest bidder to the bidding team, with a bid of 250
  --             let
  --               newState = state
  --                 { biddingTeam = [newPlayerIndex]
  --                 , bidData = (newPlayerIndex, newBid)
  --                 }

  --             updateState stateMapMVar gName newState

  --             closeBidding newPlayerIndex newBid $ gameData newState
  --           else
  --             when (newBid > oldBid) $ do
  --               -- Update the state with the new highest bid
  --               updateState stateMapMVar gName
  --                 $ state { bidData = (newPlayerIndex, newBid) }

  --               -- Also inform the players that the max bid has been updated
  --               updateMaximumBid newPlayerIndex newBid $ gameData state

  --     Nothing ->
  --       pure ()

  -- sendTrump :: SD.ReceiveSelectionData -> IO ()
  -- sendTrump receiveSelectionData = do
  --   stateMap <- readMVar stateMapMVar

  --   let gName = SD.gameName receiveSelectionData
  --   case M.lookup gName stateMap of
  --     Just state -> do
  --       let
  --         connectionList = listConnections $ gameData state
  --         sendSelectionData = SD.value receiveSelectionData

  --       putStrLn $ "Trump Selected: " ++ show (SD.selectedTrump sendSelectionData)
  --       putStrLn $ "Helper 1: " ++ show (SD.helper1 sendSelectionData)
  --       putStrLn $ "Helper 2: " ++ show (SD.helper2 sendSelectionData)

  --       -- Update the state with the received selection data
  --       updateState stateMapMVar gName
  --         $ state { selectionData = sendSelectionData }

  --       for_ connectionList $ \conn ->
  --         sendTextData conn $ encode sendSelectionData

  --     Nothing ->
  --       pure ()

  -- sendCard :: PC.ReceivePlayedCard -> IO ()
  -- sendCard (PC.RPC gName card) = do
  --   stateMap <- readMVar stateMapMVar

  --   case M.lookup gName stateMap of
  --     Just state -> do
  --       -- Send the player and played card to each player
  --       let
  --         connectionList = listConnections $ gameData state
  --         playerTurn = turn $ gameData state
  --         newHand = updateHand card playerTurn $ hand state
  --         newTurn = P.nextTurn playerTurn

  --         -- If the newly played card is a helper, add it to the bidding team
  --         -- and update the helpers revealed count
  --         (newBiddingTeam, newHelpersRevealed) =
  --           if SD.isHelper card (selectionData state)
  --             then (playerTurn : biddingTeam state, 1 + helpersRevealed state)
  --             else (biddingTeam state, helpersRevealed state)

  --         -- If all helper cards have been revealed, add remaining players to anti-team
  --         newAntiTeam =
  --           if newHelpersRevealed == SD.maxHelpers (selectionData state)
  --             then filter ( `notElem` newBiddingTeam) P.playerIndices
  --             else antiTeam state

  --         newState = state
  --           { hand = newHand
  --           , gameData = (gameData state)
  --               { turn = newTurn }
  --           , biddingTeam = newBiddingTeam
  --           , helpersRevealed = newHelpersRevealed
  --           , antiTeam = newAntiTeam
  --           }

  --       putStrLn $ "Player: " ++ P.name (getPlayer playerTurn $ players $ gameData state) ++ " has played card "
  --                 ++ show card

  --       -- Update the state with the updated hand, and newTurn
  --       updateState stateMapMVar gName newState

  --       for_ connectionList $ \conn ->
  --         sendTextData conn $ encode $ PC.SPC newTurn card

  --       -- When next turn comes back to the first player (ie the round finishes),
  --       -- then calculate the score and send it after an interval of 2 seconds
  --       when (newTurn == firstPlayer state) $
  --         void $ forkIO $ do
  --           threadDelay $ 2 * 1000 * 1000
  --           sendRoundScore gName newState newHand

  --     Nothing ->
  --       pure ()


  -- initiateGame :: String -> GameData -> IO ()
  -- initiateGame gName gData = do
  --   putStrLn "Initializing Game..."
  --   cards <- P.shuffledCards
  --   let
  --     connectionList = listConnections gData
  --   for_ (zip P.playerIndices connectionList) $ \(i, conn) ->
  --     sendTextData conn $ encode $ P.GS
  --     { P.playerSet = players gData
  --     , P.firstBidder = firstBidder gData
  --     , P.myIndex = i
  --     , P.myCards = P.getCards i cards
  --     , P.gameId = gName
  --     }

  -- updateMaximumBid :: P.PlayerIndex -> Int -> GameData -> IO ()
  -- updateMaximumBid newPlayerIndex newBid gData = do
  --   let connectionList = listConnections gData
  --   putStrLn $ "Communicating max bid: " ++ show newBid

  --   for_ connectionList $ \conn ->
  --     sendTextData conn $ encode $ SBD newPlayerIndex newBid

  -- closeBidding :: P.PlayerIndex -> Int -> GameData -> IO ()
  -- closeBidding winner maxBid gData = do
  --   putStrLn $ "Communcationg Final bid: " ++ show maxBid
  --   let connectionList = listConnections gData

  --   for_ connectionList $ \conn ->
  --     sendTextData conn $ encode $ FBD winner maxBid
  
  -- sendRoundScore :: String -> State -> Hand -> IO ()
  -- sendRoundScore gName state fullHand =
  --   -- First, get the suit of the card played by the first player
  --   case getCardFromHand (firstPlayer state) fullHand of
  --     Just (Card _ baseSuit) -> do
  --       let
  --         connectionList = listConnections $ gameData state
  --         cards = getCardsFromHand fullHand
  --         trump = SD.selectedTrump $ selectionData state

  --         -- Normally the person with the highest card of `suit` should win
  --         -- However, if someone used a trump, then the highest trump will win
  --         wasTrumpUsed = any ( (==) trump . suit . snd) cards
  --         winner =
  --           if wasTrumpUsed
  --             then
  --               fst $ maximumBy (compare `on` snd) $ filter ((==) trump . suit . snd) cards
  --             else
  --               fst $ maximumBy (compare `on` snd) $ filter ((==) baseSuit . suit . snd) cards

  --         -- Calculate the score
  --         score = sum $ map (calculateScore . snd) cards
  --         -- Add score to the player
  --         newPlayerSet = addScore score winner (players $ gameData state)
  --         -- Increment the round
  --         newRound = nextRound $ roundIndex state

  --         newState = state
  --           { gameData = (gameData state)
  --             { players = newPlayerSet
  --             , turn = winner
  --             }
  --             , firstPlayer = winner
  --             , hand = emptyHand
  --             , roundIndex = newRound
  --           }

  --       putStrLn $ "Player: " ++ P.name (getPlayer winner $ players $ gameData state) ++ " has won round with score: "
  --                 ++ show score

  --       -- Update the state with new scores, and the new first turn
  --       updateState stateMapMVar gName newState

  --       -- When the 8th round finishes, decide which team won
  --       -- And then update their scores
  --       when (newRound == Round1) $ finalizeGame gName newState

  --       for_ connectionList $ \conn ->
  --         sendTextData conn $ encode $ PC.NRD winner newPlayerSet

  --     Nothing ->
  --       pure ()

  -- finalizeGame :: String -> State -> IO ()
  -- finalizeGame gName state = do
  --   let
  --     bidAmount = snd $ bidData state
  --     biddingTeamPlayers = map ( `getPlayer` (players $ gameData state)) $ biddingTeam state
  --     biddingTeamTotal = sum $ map P.gameScore biddingTeamPlayers

  --     antiTeamPlayers = map ( `getPlayer` (players $ gameData state)) $ antiTeam state
  --     antiTeamTotal = sum $ map P.gameScore antiTeamPlayers

  --     -- Anti team won, with a score of 100+, they each get the bidAmount as score
  --     (score, winningTeam)
  --       | biddingTeamTotal >= bidAmount =
  --         (bidAmount, biddingTeam state)
  --       | antiTeamTotal >= 100 =
  --         (bidAmount, antiTeam state)
  --       | otherwise =
  --         (antiTeamTotal, antiTeam state)

  --     newPlayers = foldl' (\ps i ->
  --         let
  --           player = getPlayer i ps
  --           newPlayer = player
  --             { P.gameScore = 0
  --             , P.totalScore = P.totalScore player + if i `elem` winningTeam then score else 0
  --             }
  --         in
  --         updatePlayer i newPlayer ps
  --       ) (players $ gameData state) P.playerIndices

  --     newFirstBidder = P.nextTurn $ firstBidder $ gameData state

  --     newState = state
  --       { gameData = (gameData state)
  --         { players = newPlayers
  --         , firstBidder = newFirstBidder
  --         , turn = newFirstBidder
  --         }
  --       , bidData = (newFirstBidder, 150)
  --       , biddingPlayers = S.fromList P.playerIndices
  --       , selectionData = SD.initSelectionData
  --       , hand = emptyHand
  --       , firstPlayer = newFirstBidder
  --       , biddingTeam = []
  --       , antiTeam = []
  --       , roundIndex = Round1
  --       , helpersRevealed = 0
  --       }

  --   updateState stateMapMVar gName newState

  --   -- Initiate a new game
  --   initiateGame gName $ gameData newState