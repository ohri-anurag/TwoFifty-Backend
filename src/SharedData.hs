{-# LANGUAGE TemplateHaskell #-}

module SharedData where

import Data.Aeson.TH

import Card
import Player

-- This is the data that we receive before a game starts

-- This is the data that we receive during the bidding round

-- This is the data received from the bidding winner

-- This data is received during the actual gameplay

data ReceivedDataValue
  = IntroData
      String        -- Player Name
  | BiddingData
      PlayerIndex   -- Bidding Player
      Int           -- Bid Amount
  | SelectionData
      Suit          -- Trump Suit
      (Maybe Card)  -- Helper 1
      (Maybe Card)  -- Helper 2
  | PlayedCard
      Card          -- Which card was played

-- Need to have a string here, since I need to know which game are we talking about
data ReceivedData = ReceivedData
  String            -- Game Id
  ReceivedDataValue

data SentData
  = GameData
      PlayerNameSet -- Set of player names
      PlayerIndex   -- The first bidder in this game
      PlayerIndex   -- Your player index
      [Card]        -- Your cards
  | MaximumBid
      PlayerIndex   -- The player who made the current maximum bid
      Int           -- Bid Amount
  | FinalBid
      PlayerIndex   -- The player who made the final bid
      Int           -- Bid Amount
  | TurnData
      -- PlayerIndex   -- Who played the most recent turn
      Card          -- The card that was played
  | RoundData
      PlayerIndex   -- The player who won the latest round, and will have the next turn
      Int           -- The amount scored in the latest round
  | GameFinishedData
      [PlayerIndex] -- The winning team
      Int           -- Their score

-- JSON derivations
$(deriveJSON defaultOptions ''ReceivedDataValue)
$(deriveJSON defaultOptions ''ReceivedData)