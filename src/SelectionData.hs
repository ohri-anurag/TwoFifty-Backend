{-# LANGUAGE TemplateHaskell #-}

module SelectionData where

import Data.Aeson.TH

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

-- JSON derivations
$(deriveJSON defaultOptions ''SelectionData)
$(deriveJSON defaultOptions ''ReceiveSelectionData)