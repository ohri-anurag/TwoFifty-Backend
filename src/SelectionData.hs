{-# LANGUAGE TemplateHaskell #-}

module SelectionData where

import Data.Aeson.TH
import Data.Maybe (isJust)

import Card

data SelectionData = SD
  { selectedTrump :: Suit
  , helper1 :: Maybe Card
  , helper2 :: Maybe Card
  }


data ReceiveSelectionData = RSD
  { gameName :: String
  , value :: SelectionData
  }

initSelectionData :: SelectionData
initSelectionData = SD
  { selectedTrump = Spade
  , helper1 = Nothing
  , helper2 = Nothing
  }

sizeOfBiddingTeam :: SelectionData -> Int
sizeOfBiddingTeam selectionData =
  1 -- For the bidder
  + if isJust (helper1 selectionData) then 1 else 0
  + if isJust (helper2 selectionData) then 1 else 0

-- JSON derivations
$(deriveJSON defaultOptions ''SelectionData)
$(deriveJSON defaultOptions ''ReceiveSelectionData)