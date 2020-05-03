{-# LANGUAGE TemplateHaskell #-}

module Player where

import Data.Aeson.TH

import Card

data IntroData = ID
  { playerName :: String
  , gameName :: String
  }

data PlayerIndex
  = Player1
  | Player2
  | Player3
  | Player4
  | Player5
  | Player6
  deriving (Eq, Ord, Show, Enum)

data Player = P
  { totalScore :: Int
  , gameScore :: Int
  , name :: String
  }

data PlayerSet = PS
  { player1 :: Player
  , player2 :: Player
  , player3 :: Player
  , player4 :: Player
  , player5 :: Player
  , player6 :: Player
  }

data GameState = GS
  { playerSet :: PlayerSet
  , firstBidder :: PlayerIndex           -- This person will play the first turn, and have the first chance at bidding.
  , myIndex :: PlayerIndex
  , myCards :: [Card]
  , gameId :: String
  }

data CardDistribution = CD
  { cardSet1 :: [Card]
  , cardSet2 :: [Card]
  , cardSet3 :: [Card]
  , cardSet4 :: [Card]
  , cardSet5 :: [Card]
  , cardSet6 :: [Card]
  }

-- Initial Data
newPlayer :: String -> Player
newPlayer str = P
  { totalScore = 0
  , gameScore = 0
  , name = str
  }

initPlayerSet :: PlayerSet
initPlayerSet = PS
  { player1 = newPlayer $ show Player1
  , player2 = newPlayer $ show Player2
  , player3 = newPlayer $ show Player3
  , player4 = newPlayer $ show Player4
  , player5 = newPlayer $ show Player5
  , player6 = newPlayer $ show Player6
  }

-- Helpers
nextTurn :: PlayerIndex -> PlayerIndex
nextTurn Player6 = Player1
nextTurn p = succ p

getCards :: PlayerIndex -> CardDistribution -> [Card]
getCards index cd =
  case index of
    Player1 -> cardSet1 cd
    Player2 -> cardSet2 cd
    Player3 -> cardSet3 cd
    Player4 -> cardSet4 cd
    Player5 -> cardSet5 cd
    Player6 -> cardSet6 cd

shuffledCards :: IO CardDistribution
shuffledCards = do
  randomizedCards <- fisherYatesShuffle allCards
  
  pure $ CD
    { cardSet1 = take 8 randomizedCards
    , cardSet2 = take 8 $ drop 8 randomizedCards
    , cardSet3 = take 8 $ drop 16 randomizedCards
    , cardSet4 = take 8 $ drop 24 randomizedCards
    , cardSet5 = take 8 $ drop 32 randomizedCards
    , cardSet6 = take 8 $ drop 40 randomizedCards
    }

playerIndices :: [PlayerIndex]
playerIndices = [Player1 .. Player6]

-- JSON derivations
$(deriveJSON defaultOptions ''IntroData)
$(deriveJSON defaultOptions ''PlayerIndex)
$(deriveJSON defaultOptions ''Player)
$(deriveJSON defaultOptions ''PlayerSet)
$(deriveJSON defaultOptions ''GameState)
