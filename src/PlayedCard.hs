{-# LANGUAGE TemplateHaskell #-}

module PlayedCard where

import Data.Aeson.TH

import Card
import Player

data ReceivePlayedCard = RPC
  { gameName :: String
  , card :: Card
  }

data SendPlayedCard = SPC
  { turn :: PlayerIndex
  , playedCard :: Card
  }

data NextRoundData = NRD
  { firstPlayer :: PlayerIndex
  , playerSet :: PlayerSet
  }

-- JSON derivations
$(deriveJSON defaultOptions ''ReceivePlayedCard)
$(deriveJSON defaultOptions ''SendPlayedCard)
$(deriveJSON defaultOptions ''NextRoundData)