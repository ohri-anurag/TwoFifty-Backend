{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}

module SharedData where

import Data.Aeson -- ((.=), (.:), FromJSON, object, Object, parseJSON, toJSON, ToJSON)
import Data.Aeson.Types
-- import Data.Aeson.TH
import Data.Text

import Card
import Player



data ReceivedDataValue
  = IntroData
      Text          -- Player Name
  | QuitBidding
      PlayerIndex   -- The player who quit bidding
  | BiddingData
      PlayerIndex   -- Bidding Player
      Int           -- Bid Amount
  | SelectionData
      Suit          -- Trump Suit
      (Maybe Card)  -- Helper 1
      (Maybe Card)  -- Helper 2
  | PlayedCard
      Card          -- Which card was played
  deriving Show

data PlayerNameSet = PlayerNameSet
  { name1 :: Text
  , name2 :: Text
  , name3 :: Text
  , name4 :: Text
  , name5 :: Text
  , name6 :: Text
  }

-- Need to have a string here, since I need to know which game are we talking about
data ReceivedData = ReceivedData
  Text            -- Game Id
  ReceivedDataValue

data SentData
  = GameData
      PlayerNameSet -- Set of player names
      PlayerIndex   -- The first bidder in this game
      PlayerIndex   -- Your player index
      [Card]        -- Your cards
  | HasQuitBidding
      PlayerIndex   -- The player who quit bidding
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
instance FromJSON ReceivedDataValue where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag of
      String str ->
        case str of
          "IntroData" -> IntroData <$> o .: "playerName"
          "QuitBidding" -> QuitBidding <$> o .: "playerIndex"
          "BiddingData" -> BiddingData <$> o .: "playerIndex" <*> o .: "bidAmount"
          "SelectionData" -> SelectionData <$> o .: "trumpSuit" <*> o .: "helper1" <*> o .: "helper2"
          "PlayedCard" -> PlayedCard <$> o .: "playedCard"
          _ -> fail $ "Unexpected tag string received: " ++ unpack str
      x ->
        prependFailure "Tag was not a string: " $
          typeMismatch "String" x

  parseJSON x =
    prependFailure "Missing tag field in ReceivedDataValue: " $
      typeMismatch "Object" x

instance FromJSON ReceivedData where
  parseJSON (Object o) = ReceivedData <$> o .: "gameName" <*> o .: "value"
  parseJSON x =
    prependFailure "Parsing ReceivedData failed, " $
      typeMismatch "Object" x

instance ToJSON PlayerNameSet where
  toJSON playerNames = object
    [ "name1" .= name1 playerNames
    , "name2" .= name2 playerNames
    , "name3" .= name3 playerNames
    , "name4" .= name4 playerNames
    , "name5" .= name5 playerNames
    , "name6" .= name6 playerNames
    ]

instance ToJSON SentData where
  toJSON (GameData playerNames firstBidder myIndex myCards) = object
    [ "playerNames" .= playerNames
    , "firstBidder" .= firstBidder
    , "myIndex" .= myIndex
    , "myCards" .= myCards
    ]
  toJSON (HasQuitBidding playerIndex) = object
    [ "hasQuitBidding" .= playerIndex ]
  toJSON (MaximumBid playerIndex bidAmount) = object
    [ "maximumBid" .= bidAmount
    , "bidder" .= playerIndex
    ]
  toJSON (FinalBid playerIndex bidAmount) = object
    [ "finalBid" .= bidAmount
    , "bidder" .= playerIndex
    ]
  toJSON (TurnData playedCard) = object
    [ "playedCard" .= playedCard ]
  toJSON (RoundData roundWinner score) = object
    [ "roundWinner" .= roundWinner
    , "roundScore" .= score
    ]
  toJSON (GameFinishedData winningTeam gameScore) = object
    [ "winningTeam" .= winningTeam
    , "gameScore" .= gameScore
    ]



-- $(deriveJSON defaultOptions ''ReceivedDataValue)
-- $(deriveJSON defaultOptions ''ReceivedData)