{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Player where

import Data.Aeson ((.=), object, toJSON, ToJSON)
import Data.Aeson.TH
import Data.List (foldl', sort)
import qualified Data.Text as T

import Card

data PlayerIndex
  = Player1
  | Player2
  | Player3
  | Player4
  | Player5
  | Player6
  deriving (Eq, Ord, Show, Enum)

data CardDistribution = CD
  { cardSet1 :: [Card]
  , cardSet2 :: [Card]
  , cardSet3 :: [Card]
  , cardSet4 :: [Card]
  , cardSet5 :: [Card]
  , cardSet6 :: [Card]
  }

data PlayerNameSet = PlayerNameSet
  { name1 :: T.Text
  , name2 :: T.Text
  , name3 :: T.Text
  , name4 :: T.Text
  , name5 :: T.Text
  , name6 :: T.Text
  }

data PlayerScores = PlayerScores
  { score1 :: Int
  , score2 :: Int
  , score3 :: Int
  , score4 :: Int
  , score5 :: Int
  , score6 :: Int
  }

setPlayerName :: (PlayerIndex, T.Text) -> PlayerNameSet -> PlayerNameSet
setPlayerName (playerIndex, name) playerNameSet =
  case playerIndex of
    Player1 -> playerNameSet { name1 = name }
    Player2 -> playerNameSet { name2 = name }
    Player3 -> playerNameSet { name3 = name }
    Player4 -> playerNameSet { name4 = name }
    Player5 -> playerNameSet { name5 = name }
    Player6 -> playerNameSet { name6 = name }

initialisePlayerNameSet :: [(PlayerIndex, T.Text)] -> PlayerNameSet
initialisePlayerNameSet =
  foldl' (flip setPlayerName) emptyNameSet
  where
    emptyNameSet = PlayerNameSet
      { name1 = ""
      , name2 = ""
      , name3 = ""
      , name4 = ""
      , name5 = ""
      , name6 = ""
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
    { cardSet1 = sort $ take 8 randomizedCards
    , cardSet2 = sort $ take 8 $ drop 8 randomizedCards
    , cardSet3 = sort $ take 8 $ drop 16 randomizedCards
    , cardSet4 = sort $ take 8 $ drop 24 randomizedCards
    , cardSet5 = sort $ take 8 $ drop 32 randomizedCards
    , cardSet6 = sort $ take 8 $ drop 40 randomizedCards
    }

playerIndices :: [PlayerIndex]
playerIndices = [Player1 .. Player6]

zeroScores :: PlayerScores
zeroScores = PlayerScores
  { score1 = 0
  , score2 = 0
  , score3 = 0
  , score4 = 0
  , score5 = 0
  , score6 = 0
  }

getScore :: PlayerIndex -> PlayerScores -> Int
getScore playerIndex playerScores =
  case playerIndex of
    Player1 -> score1 playerScores
    Player2 -> score2 playerScores
    Player3 -> score3 playerScores
    Player4 -> score4 playerScores
    Player5 -> score5 playerScores
    Player6 -> score6 playerScores

updateScore :: PlayerIndex -> Int -> PlayerScores -> PlayerScores
updateScore playerIndex score playerScores =
  case playerIndex of
    Player1 -> playerScores { score1 = score + score1 playerScores }
    Player2 -> playerScores { score2 = score + score2 playerScores }
    Player3 -> playerScores { score3 = score + score3 playerScores }
    Player4 -> playerScores { score4 = score + score4 playerScores }
    Player5 -> playerScores { score5 = score + score5 playerScores }
    Player6 -> playerScores { score6 = score + score6 playerScores }

$(deriveJSON defaultOptions ''PlayerIndex)

instance ToJSON PlayerNameSet where
  toJSON playerNames = object
    [ "name1" .= name1 playerNames
    , "name2" .= name2 playerNames
    , "name3" .= name3 playerNames
    , "name4" .= name4 playerNames
    , "name5" .= name5 playerNames
    , "name6" .= name6 playerNames
    ]