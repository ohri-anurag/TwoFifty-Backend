{-# LANGUAGE TemplateHaskell #-}
module State where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Data.Aeson.TH
import qualified Data.Map as M
import Data.Text (Text)
import Network.WebSockets.Connection (Connection)

import Card
import Player
import qualified Game as G

-- Now that we can distinguish between the types of data coming in,
-- we can also keep different state types

data CommonStateData = CommonStateData
  { firstBidder :: PlayerIndex          -- First Bidder
  , playerDataSet :: PlayerDataSet
  , bidder :: PlayerIndex               -- Current maximum bidder
  , bid :: Int                          -- Maximum bid amount
  }

data RoundStateData = RoundStateData
  { roundIndex :: Round               -- Which round?
  , trumpSuit :: Suit                 -- Selected trump
  , helperCards :: [Card]             -- Max 2 cards selected by the bidder
  , biddingTeam :: [PlayerIndex]      -- Bidding Team
  , firstPlayer :: PlayerIndex        -- Who had the first turn
  , currentTurn :: PlayerIndex        -- Current turn
  }

data State
  = IntroState              -- When players are still connecting, we store (player names, player ids), and game name
      [((Text, Text), Connection)]  -- List of player names, MAX = 6
  | BiddingState
      CommonStateData
      [PlayerIndex]         -- Bidders
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

data MyState = MyState StateMap [G.Game]

updateState :: MVar MyState -> Text -> State -> IO ()
updateState myStateMVar gName state  = do
  (MyState stateMap games) <- takeMVar myStateMVar
  putMVar myStateMVar
    $ MyState (M.insert gName state stateMap) games

updateGames :: MVar MyState -> G.Game -> IO ()
updateGames myStateMVar game = do
  (MyState stateMap games) <- takeMVar myStateMVar
  putMVar myStateMVar
    $ MyState stateMap
    $ game : games

nextRound :: Round -> Round
nextRound Round1 = Round2
nextRound Round2 = Round3
nextRound Round3 = Round4
nextRound Round4 = Round5
nextRound Round5 = Round6
nextRound Round6 = Round7
nextRound Round7 = Round8
nextRound Round8 = Round1

$(deriveJSON defaultOptions ''Round)