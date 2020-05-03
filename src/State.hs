module State where

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import qualified Data.Map as M
import qualified Data.Set as S

import Game
import qualified Player as P
import SelectionData

data State = State
  { gameData :: GameData
  , bidData :: (P.PlayerIndex, Int)
  , biddingPlayers :: S.Set P.PlayerIndex
  , selectionData :: SelectionData
  }

type StateMap = M.Map String State

updateState :: MVar StateMap -> String -> State -> IO ()
updateState stateMapMVar gName state  = do
  stateMap <- takeMVar stateMapMVar
  putMVar stateMapMVar
    $ M.insert gName state stateMap

removePlayerFromBiddingSet :: P.PlayerIndex -> State -> State
removePlayerFromBiddingSet index state = state
  { biddingPlayers = S.delete index $ biddingPlayers state }

isBiddingCompleted :: State -> Bool
isBiddingCompleted state =
  S.null $ biddingPlayers state
