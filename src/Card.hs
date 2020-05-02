{-# LANGUAGE TemplateHaskell #-}

module Card where

import Data.Aeson.TH

data Suit
  = Club
  | Heart
  | Diamond
  | Spade
  deriving (Eq, Ord, Enum)

data CardValue
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq, Ord, Enum)

data Card = Card
  { value :: CardValue
  , suit :: Suit
  }
  deriving (Eq, Ord)

suits :: [Suit]
suits = [Club .. Spade]

values :: [CardValue]
values = [Ace .. King]

allCards :: [Card]
allCards = [Card v s | s <- suits, v <- values, v /= Two]

$(deriveJSON defaultOptions ''Suit)
$(deriveJSON defaultOptions ''CardValue)
$(deriveJSON defaultOptions ''Card)