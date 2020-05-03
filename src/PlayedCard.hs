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

-- data RoundFinished = RF
--   { winner :: PlayerIndex
--   , playerSet :: PlayerSet
--   }

-- JSON derivations
$(deriveJSON defaultOptions ''ReceivePlayedCard)
$(deriveJSON defaultOptions ''SendPlayedCard)