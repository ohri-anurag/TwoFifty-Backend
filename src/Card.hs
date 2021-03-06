{-# LANGUAGE TemplateHaskell #-}

module Card where

import Data.Aeson.TH
import Data.Foldable (foldlM)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import System.Random (getStdRandom, randomR)

data Suit
  = Spade
  | Heart
  | Club
  | Diamond
  deriving (Eq, Ord, Show, Enum)

data CardValue
  = Two
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
  | Ace
  deriving (Eq, Ord, Show, Enum)

data Card = Card
  { value :: CardValue
  , suit :: Suit
  }
  deriving Eq

instance Show Card where
  show (Card v s) = show v ++ " of " ++ show s

instance Ord Card where
  compare (Card v1 s1) (Card v2 s2)
    | s1 == s2 =
      compare v1 v2

    | otherwise =
      compare s1 s2

calculateScore :: Card -> Int
calculateScore (Card Three Spade) = 30
calculateScore (Card val _)
  | val >= Ten =
    10

  | val == Five =
    5

  | otherwise =
    0

suits :: [Suit]
suits = [Spade .. Diamond]

values :: [CardValue]
values = [Three .. Ace]

allCards :: [Card]
allCards = [Card v s | s <- suits, v <- values]

fisherYatesShuffle :: [a] -> IO [a]
fisherYatesShuffle items =
  M.elems <$> foldlM func mapItems [1..(n-1)]
  where
    func mp i = do
      j <- getStdRandom (randomR (i, n))
      pure $ swap i j mp

    mapItems = M.fromList $ zip [1..] items
    n = M.size mapItems
    swap i j mp =
      let
        vi = fromJust $ M.lookup i mp
        vj = fromJust $ M.lookup j mp
      in
        M.insert i vj (M.insert j vi mp)

-- JSON derivations
$(deriveJSON defaultOptions ''Suit)
$(deriveJSON defaultOptions ''CardValue)
$(deriveJSON defaultOptions ''Card)
