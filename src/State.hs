module State where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import qualified Data.Map as M
import Data.Text (Text)
import Network.WebSockets.Connection (Connection)

import Card
import Player

-- Now that we can distinguish between the types of data coming in,
-- we can also keep different state types

data CommonStateData = CommonStateData
  { firstBidder :: PlayerIndex          -- First Bidder
  , playerDataSet :: PlayerDataSet
  }

data BiddingStateData = BiddingStateData
  { bidders :: [PlayerIndex]                -- Number of bidders
  , highestBidder :: PlayerIndex            -- Current maximum bidder
  , highestBid :: Int                       -- Maximum bid amount
  }

data RoundStateData = RoundStateData
  { roundIndex :: Round               -- Which round?
  , trumpSuit :: Suit                 -- Selected trump
  , biddingTeam :: [PlayerIndex]      -- Bidding Team
  , firstPlayer :: PlayerIndex        -- Who had the first turn
  , currentTurn :: PlayerIndex        -- Current turn
  , hand ::                           -- The cards being played
      M.Map PlayerIndex Card
  , bid :: Int
  }

data State
  = IntroState              -- When players are still connecting, we store (player names, player ids), and game name
      [((Text, Text), Connection)]  -- List of player names, MAX = 6
  | BiddingState
      CommonStateData
      BiddingStateData
  | RoundState
      CommonStateData
      RoundStateData

data Round
  = Round1
  | Round2
  | Round3
  | Round4
  | Round5
  | Round6
  | Round7
  | Round8
  deriving (Eq, Show)

type StateMap = M.Map Text State

updateState :: MVar StateMap -> Text -> State -> IO ()
updateState stateMapMVar gName state  = do
  stateMap <- takeMVar stateMapMVar
  putMVar stateMapMVar
    $ M.insert gName state stateMap

nextRound :: Round -> Round
nextRound Round1 = Round2
nextRound Round2 = Round3
nextRound Round3 = Round4
nextRound Round4 = Round5
nextRound Round5 = Round6
nextRound Round6 = Round7
nextRound Round7 = Round8
nextRound Round8 = Round1