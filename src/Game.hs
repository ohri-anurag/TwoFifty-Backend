module Game where

import Data.Maybe (mapMaybe)
import Network.WebSockets.Connection (Connection)

import Player (Player, PlayerIndex(..), PlayerSet(..), initPlayerSet)

data ConnectionSet = CS
  { connection1 :: Maybe Connection
  , connection2 :: Maybe Connection
  , connection3 :: Maybe Connection
  , connection4 :: Maybe Connection
  , connection5 :: Maybe Connection
  , connection6 :: Maybe Connection
  }

data GameData = GD
  { players :: PlayerSet
  , connections :: ConnectionSet
  , nextPlayerIndex :: Maybe PlayerIndex  -- Used to fill up a game, when this becomes Just Player6, this means game is full.
  , firstBidder :: PlayerIndex
  }

-- Update functions
updatePlayer :: PlayerIndex -> Player -> PlayerSet -> PlayerSet
updatePlayer index player playerSet =
  case index of
    Player1 -> playerSet { player1 = player }
    Player2 -> playerSet { player2 = player }
    Player3 -> playerSet { player3 = player }
    Player4 -> playerSet { player4 = player }
    Player5 -> playerSet { player5 = player }
    Player6 -> playerSet { player6 = player }

updateConnection :: PlayerIndex -> Connection -> ConnectionSet -> ConnectionSet
updateConnection index connection connectionSet =
  case index of
    Player1 -> connectionSet { connection1 = Just connection }
    Player2 -> connectionSet { connection2 = Just connection }
    Player3 -> connectionSet { connection3 = Just connection }
    Player4 -> connectionSet { connection4 = Just connection }
    Player5 -> connectionSet { connection5 = Just connection }
    Player6 -> connectionSet { connection6 = Just connection }

addPlayerAndConnection :: Player -> Connection -> GameData -> GameData
addPlayerAndConnection player connection gameData = gameData
  { players = updatePlayer nextIndex player $ players gameData
  , connections = updateConnection nextIndex connection $ connections gameData
  , nextPlayerIndex = Just nextIndex
  }
  where
    nextIndex = maybe Player1 succ $ nextPlayerIndex gameData

-- Initial Data
initConnectionSet :: ConnectionSet
initConnectionSet = CS
  { connection1 = Nothing
  , connection2 = Nothing
  , connection3 = Nothing
  , connection4 = Nothing
  , connection5 = Nothing
  , connection6 = Nothing
  }

initGameData :: GameData
initGameData = GD
  { players = initPlayerSet
  , connections = initConnectionSet
  , nextPlayerIndex = Nothing
  , firstBidder = Player1
  }

-- Helpers
listConnections :: GameData -> [Connection]
listConnections gameData =
  mapMaybe ($ connections gameData)
    [ connection1
    , connection2
    , connection3
    , connection4
    , connection5
    , connection6
    ]
