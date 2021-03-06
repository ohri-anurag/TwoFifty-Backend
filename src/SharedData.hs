{-# LANGUAGE OverloadedStrings #-}

module SharedData where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Card
import Player
import State

data SelectionData =
  SelectionData
    Suit          -- Trump Suit
    [Card]        -- Helpers
  deriving Show

data ReceivedDataValue
  = IntroData
      T.Text          -- Player Name
      T.Text          -- Player Id
  | QuitBidding
      PlayerIndex   -- The player who quit bidding
  | IncreaseBid
      PlayerIndex   -- Bidding Player
      Int           -- Bid Amount
  | ReceivedSelectionData
      SelectionData
  | PlayedCard
      Card          -- Which card was played
  deriving Show

-- Need to have a string here, since I need to know which game are we talking about
data ReceivedData = ReceivedData
  T.Text            -- Game Id
  ReceivedDataValue

data SentData
  = PlayerJoined
      T.Text          -- Newly Joined Player
  | ExistingPlayers
      [T.Text]        -- Already existing players in the game
  | GameData
      [(PlayerIndex, T.Text)]   -- Player names
      PlayerIndex             -- The first bidder in this game
      PlayerIndex             -- Your player index
      [Card]                  -- Your cards
  | HasQuitBidding
      PlayerIndex   -- The player who quit bidding
  | MaximumBid
      PlayerIndex   -- The player who made the current maximum bid
      Int           -- Bid Amount
  | SentSelectionData
      SelectionData
  | PlayCard
      Card          -- The card that was played
  | RoundData
      PlayerIndex   -- The player who won the latest round, and will have the next turn
      Int           -- The amount scored in the latest round
  | GameFinishedData
      [PlayerIndex] -- The winning team
      Int           -- Their score
  | NewGame
      [Card]        -- Cards for new game
  | BiddingReconnectionData
      PlayerIndex
      CommonStateData
      [PlayerIndex]     -- Bidders
  | RoundReconnectionData
      PlayerIndex
      CommonStateData
      RoundStateData
  | PlayerWithIdAlreadyExists
  | PlayerWithNameAlreadyExists



-- JSON derivations
instance FromJSON ReceivedDataValue where
  parseJSON (Object o) = do
    tag <- o .: "tag"
    case tag of
      String str ->
        case str of
          "IntroData" -> IntroData <$> o .: "playerName" <*> o .: "playerId"
          "QuitBidding" -> QuitBidding <$> o .: "quitter"
          "IncreaseBid" -> IncreaseBid <$> o .: "bidder" <*> o .: "bid"
          "SelectionData" -> ReceivedSelectionData <$> (SelectionData <$> o .: "trump" <*> o .: "helpers")
          "PlayedCard" -> PlayedCard <$> o .: "playedCard"
          _ -> fail $ "Unexpected tag string received: " ++ T.unpack str
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

instance ToJSON SentData where
  toJSON sentData@(PlayerJoined playerName) = object
    [ "tag" .= tagName sentData
    , "newPlayer" .= playerName
    ]
  toJSON sentData@(ExistingPlayers playerNames) = object
    [ "tag" .= tagName sentData
    , "existingPlayers" .= playerNames
    ]
  toJSON sentData@(GameData playerNames firstBidderIndex myIndex myCards) = object
    [ "tag" .= tagName sentData
    , "playerNames" .= object (map (\(i, n) -> T.pack (show i) .= n) playerNames)
    , "firstBidder" .= firstBidderIndex
    , "myIndex" .= myIndex
    , "myCards" .= myCards
    ]
  toJSON sentData@(HasQuitBidding playerIndex) = object
    [ "tag" .= tagName sentData
    , "hasQuitBidding" .= playerIndex
    ]
  toJSON sentData@(MaximumBid playerIndex bidAmount) = object
    [ "tag" .= tagName sentData
    , "highestBid" .= bidAmount
    , "highestBidder" .= playerIndex
    ]
  toJSON sentData@(SentSelectionData (SelectionData trump helpers)) = object
    [ "tag" .= tagName sentData
    , "trump" .= trump
    , "helpers" .= helpers
    ]
  toJSON sentData@(PlayCard playedCard) = object
    [ "tag" .= tagName sentData
    , "card" .= playedCard
    ]
  toJSON sentData@(RoundData roundWinner myScore) = object
    [ "tag" .= tagName sentData
    , "roundWinner" .= roundWinner
    , "roundScore" .= myScore
    ]
  toJSON sentData@(GameFinishedData winningTeam winningScore) = object
    [ "tag" .= tagName sentData
    , "winningTeam" .= winningTeam
    , "gameScore" .= winningScore
    ]
  toJSON sentData@(NewGame myCards) = object
    [ "tag" .= tagName sentData
    , "cards" .= myCards
    ]
  toJSON sentData@(BiddingReconnectionData myIndex commonStateData bidders) = object $
    [ "tag" .= tagName sentData
    , "bidders" .= bidders
    ]
    ++ commonJSON myIndex commonStateData
  toJSON sentData@(RoundReconnectionData myIndex commonStateData roundStateData) = object $
    [ "tag" .= tagName sentData
    , "selectionData" .= object
      [ "trump" .= trumpSuit roundStateData
      , "helpers" .= helperCards roundStateData
      ]
    , "firstPlayer" .= firstPlayer roundStateData
    , "turn" .= currentTurn roundStateData
    , "round" .= roundIndex roundStateData
    ]
    ++ commonJSON myIndex commonStateData
  toJSON sentData = object
    [ "tag" .= tagName sentData ]

commonJSON :: PlayerIndex -> CommonStateData -> [Pair]
commonJSON myIndex commonStateData =
  [ "playerSet" .= object (map (\(i, n) -> T.pack (show i) .= n) players)
  , "biddingData" .= object
    [ "highestBid" .= bid commonStateData
    , "highestBidder" .= bidder commonStateData
    , "firstBidder" .= firstBidder commonStateData
    ]
  , "myData" .= object
    [ "myIndex" .= myIndex
    , "myCards" .= currentCards (snd $ head $ filter ((==) myIndex . fst) players)
    ]
  ]
  where players = toList $ playerDataSet commonStateData

tagName :: SentData -> T.Text
tagName (PlayerJoined _) = "PlayerJoined"
tagName (ExistingPlayers _) = "ExistingPlayers"
tagName GameData {} = "GameData"
tagName (HasQuitBidding _) = "HasQuitBidding"
tagName (MaximumBid _ _) = "MaximumBid"
tagName (SentSelectionData _) = "SelectionData"
tagName (PlayCard _) = "PlayCard"
tagName (RoundData _ _) = "RoundData"
tagName (GameFinishedData _ _) = "GameFinishedData"
tagName (NewGame _) = "NewGame"
tagName BiddingReconnectionData {} = "BiddingReconnectionData"
tagName RoundReconnectionData {} = "RoundReconnectionData"
tagName PlayerWithIdAlreadyExists = "PlayerWithIdAlreadyExists"
tagName PlayerWithNameAlreadyExists = "PlayerWithNameAlreadyExists"