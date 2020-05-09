module State where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import qualified Data.Map as M
-- import Data.Maybe (mapMaybe)
-- import qualified Data.Set as S
import Data.Text (Text)
import Network.WebSockets.Connection (Connection)

import Card
-- import Game
import Player
-- import SelectionData

-- Now that we can distinguish between the types of data coming in,
-- we can also keep different state types

data State
  = IntroState              -- When players are still connecting, we store player names, and game name
      [(Text, Connection)]  -- List of player names, MAX = 6
  | BiddingState
      PlayerNameSet     -- Names of players assigned to indices
      CardDistribution  -- The cards assigned to each player
      (M.Map            -- Each player's connection, that will be used to send messages
        PlayerIndex
        Connection)
      PlayerIndex       -- First bidder
      PlayerIndex       -- Current maximum bidder
      Int               -- Maximum bid amount
  | RoundState
      Round             -- Which round?
      Suit              -- Selected trump
      PlayerIndex       -- First Bidder
      [PlayerIndex]     -- Bidding Team
      PlayerIndex       -- Current turn
      (M.Map            -- The cards being played
        PlayerIndex
        Card)

-- data Hand = Hand
--   { card1 :: Maybe Card
--   , card2 :: Maybe Card
--   , card3 :: Maybe Card
--   , card4 :: Maybe Card
--   , card5 :: Maybe Card
--   , card6 :: Maybe Card
--   }

data Round
  = Round1
  | Round2
  | Round3
  | Round4
  | Round5
  | Round6
  | Round7
  | Round8
  deriving Eq

-- data State = State
--   { gameData :: GameData
--   , bidData :: (P.PlayerIndex, Int)
--   , biddingPlayers :: S.Set P.PlayerIndex
--   , selectionData :: SelectionData
--   , hand :: Hand
--   , firstPlayer :: P.PlayerIndex
--   , biddingTeam :: [P.PlayerIndex]
--   , antiTeam :: [P.PlayerIndex]
--   , roundIndex :: Round
--   , helpersRevealed :: Int
--   }

type StateMap = M.Map Text State

-- emptyHand :: Hand
-- emptyHand = Hand
--   { card1 = Nothing
--   , card2 = Nothing
--   , card3 = Nothing
--   , card4 = Nothing
--   , card5 = Nothing
--   , card6 = Nothing
--   }

-- getCardsFromHand :: Hand -> [(P.PlayerIndex, Card)]
-- getCardsFromHand h =
--   zip P.playerIndices
--   $ mapMaybe (`getCardFromHand` h) P.playerIndices

-- updateHand :: Card -> P.PlayerIndex -> Hand -> Hand
-- updateHand card playerIndex oldHand =
--   case playerIndex of
--     P.Player1 ->
--       oldHand { card1 = Just card }

--     P.Player2 ->
--       oldHand { card2 = Just card }

--     P.Player3 ->
--       oldHand { card3 = Just card }

--     P.Player4 ->
--       oldHand { card4 = Just card }

--     P.Player5 ->
--       oldHand { card5 = Just card }

--     P.Player6 ->
--       oldHand { card6 = Just card }

-- getCardFromHand :: P.PlayerIndex -> Hand -> Maybe Card
-- getCardFromHand playerIndex oldHand =
--   case playerIndex of
--     P.Player1 ->
--       card1 oldHand

--     P.Player2 ->
--       card2 oldHand

--     P.Player3 ->
--       card3 oldHand

--     P.Player4 ->
--       card4 oldHand

--     P.Player5 ->
--       card5 oldHand

--     P.Player6 ->
--       card6 oldHand

updateState :: MVar StateMap -> Text -> State -> IO ()
updateState stateMapMVar gName state  = do
  stateMap <- takeMVar stateMapMVar
  putMVar stateMapMVar
    $ M.insert gName state stateMap

-- removePlayerFromBiddingSet :: P.PlayerIndex -> State -> State
-- removePlayerFromBiddingSet index state = state
--   { biddingPlayers = S.delete index $ biddingPlayers state }

-- isBiddingCompleted :: State -> Bool
-- isBiddingCompleted state =
--   S.null $ biddingPlayers state

nextRound :: Round -> Round
nextRound Round1 = Round2
nextRound Round2 = Round3
nextRound Round3 = Round4
nextRound Round4 = Round5
nextRound Round5 = Round6
nextRound Round6 = Round7
nextRound Round7 = Round8
nextRound Round8 = Round1