{-# LANGUAGE TemplateHaskell #-}
module Game where

import Data.Aeson.TH
import Data.Text (Text)

import Card

data Game = Game
  { name :: Text
  , bid :: Int
  , score :: Int
  , trump :: Suit
  , helpers :: [Card]
  , bidTeam :: [(Text, Text)]
  , antiTeam :: [(Text, Text)]
  }

$(deriveJSON defaultOptions ''Game)