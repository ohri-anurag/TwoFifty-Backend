{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Player where

import Data.Aeson ((.=), object, toJSON, ToJSON)
import Data.Aeson.TH
import Data.Foldable (traverse_)
import Data.List (sort) -- , splitAt)
import qualified Data.Text as T
import Network.WebSockets.Connection (Connection)
import Prelude hiding (id)

import Card

data PlayerIndex
  = Player1
  | Player2
  | Player3
  | Player4
  | Player5
  | Player6
  deriving (Eq, Ord, Show, Enum)

data PlayerData = PlayerData
  { name :: T.Text
  , id :: T.Text
  , totalScore :: Int
  , gameScore :: Int
  , connection :: Connection
  , cards :: [Card]
  }

data PlayerDataSet = DataSet
  { player1 :: PlayerData
  , player2 :: PlayerData
  , player3 :: PlayerData
  , player4 :: PlayerData
  , player5 :: PlayerData
  , player6 :: PlayerData
  }

getPlayer :: PlayerIndex -> PlayerDataSet -> PlayerData
getPlayer playerIndex playerDataSet = case playerIndex of
  Player1 -> player1 playerDataSet
  Player2 -> player2 playerDataSet
  Player3 -> player3 playerDataSet
  Player4 -> player4 playerDataSet
  Player5 -> player5 playerDataSet
  Player6 -> player6 playerDataSet

toList :: PlayerDataSet -> [(PlayerIndex, PlayerData)]
toList psd = zip playerIndices [player1 psd, player2 psd, player3 psd, player4 psd, player5 psd, player6 psd]

foldrIndex :: (PlayerIndex -> PlayerData -> b -> b) -> b -> PlayerDataSet -> b
foldrIndex f z ds = foldr (uncurry f) z $ toList ds

forIndex_ :: Applicative t => PlayerDataSet -> (PlayerIndex -> PlayerData -> t ()) -> t ()
forIndex_ ds f = traverse_ (uncurry f) $ toList ds

updatePlayerData :: PlayerIndex -> (PlayerData -> PlayerData) -> PlayerDataSet -> PlayerDataSet
updatePlayerData playerIndex updater playerDataSet = case playerIndex of
  Player1 -> playerDataSet { player1 = updater $ player1 playerDataSet }
  Player2 -> playerDataSet { player2 = updater $ player2 playerDataSet }
  Player3 -> playerDataSet { player3 = updater $ player3 playerDataSet }
  Player4 -> playerDataSet { player4 = updater $ player4 playerDataSet }
  Player5 -> playerDataSet { player5 = updater $ player5 playerDataSet }
  Player6 -> playerDataSet { player6 = updater $ player6 playerDataSet }

updateTotalScoreAndCards :: PlayerIndex -> (Int, [Card]) -> PlayerDataSet -> PlayerDataSet
updateTotalScoreAndCards playerIndex (myScore, myCards) = updatePlayerData playerIndex
  (\p -> p
    { totalScore = myScore + totalScore p
    , cards = myCards
    , gameScore = 0
    }
  )

updateGameScore :: PlayerIndex -> Int -> PlayerDataSet -> PlayerDataSet
updateGameScore playerIndex myScore = updatePlayerData playerIndex (\p -> p { gameScore = myScore + gameScore p })

updateCards :: PlayerIndex -> Card -> PlayerDataSet -> PlayerDataSet
updateCards playerIndex card = updatePlayerData playerIndex (\p -> p { cards = filter (/= card) $ cards p })

fromIntroData :: [([Card], ((T.Text, T.Text), Connection))] -> PlayerDataSet
fromIntroData list = DataSet
  { player1 = createPlayerData $ head list
  , player2 = createPlayerData $ list !! 1
  , player3 = createPlayerData $ list !! 2
  , player4 = createPlayerData $ list !! 3
  , player5 = createPlayerData $ list !! 4
  , player6 = createPlayerData $ list !! 5
  }
  where
    createPlayerData :: ([Card], ((T.Text, T.Text), Connection)) -> PlayerData
    createPlayerData (myCards, ((playerName, playerId), conn)) = PlayerData
      { name = playerName
      , id = playerId
      , totalScore = 0
      , gameScore = 0
      , connection = conn
      , cards = myCards
      }

-- Helpers
nextTurn :: PlayerIndex -> PlayerIndex
nextTurn Player6 = Player1
nextTurn p = succ p

shuffledCards :: IO [[Card]]
shuffledCards = do
  randomizedCards <- fisherYatesShuffle allCards

  pure $ helper randomizedCards
  where
    helper [] = []
    helper cs = 
      let (mine, others) = splitAt 8 cs
      in
        sort mine : helper others

playerIndices :: [PlayerIndex]
playerIndices = [Player1 .. Player6]

$(deriveJSON defaultOptions ''PlayerIndex)

instance ToJSON PlayerData where
  toJSON playerData = object
    [ "name" .= name playerData
    , "gameScore" .= gameScore playerData
    , "totalScore" .= totalScore playerData
    ]
