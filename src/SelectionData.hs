-- {-# LANGUAGE TemplateHaskell #-}

module SelectionData where

-- import Data.Aeson.TH

-- import Card

-- initSelectionData :: SelectionData
-- initSelectionData = SD
--   { selectedTrump = Spade
--   , helper1 = Nothing
--   , helper2 = Nothing
--   }

-- isHelper :: Card -> SelectionData -> Bool
-- isHelper card selectionData =
--   maybe False (card == ) (helper1 selectionData)
--   || maybe False (card == ) (helper2 selectionData)

-- maxHelpers :: SelectionData -> Int
-- maxHelpers selectionData =
--   maybe 0 (const 1) (helper1 selectionData)
--   + maybe 0 (const 1) (helper2 selectionData)

-- -- JSON derivations
-- $(deriveJSON defaultOptions ''SelectionData)
-- $(deriveJSON defaultOptions ''ReceiveSelectionData)