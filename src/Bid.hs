{-# LANGUAGE TemplateHaskell #-}

module Bid where

import Data.Aeson.TH

import Player

-- data SendBiddingData = SBD
--   { highestBidder :: PlayerIndex
--   , highestBid :: Int
--   }

-- data FinalBiddingData = FBD
--   { biddingWinner :: PlayerIndex
--   , winningBid :: Int
--   }

-- -- JSON derivations
-- $(deriveJSON defaultOptions ''ReceiveBiddingData)
-- $(deriveJSON defaultOptions ''SendBiddingData)
-- $(deriveJSON defaultOptions ''FinalBiddingData)