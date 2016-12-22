{-# LANGUAGE FlexibleContexts #-}
module Day6 where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Tuple
import           Data.List
import           Data.Ord

type ECMap = Map Int (Map Char Int)

iter :: ECMap -> String -> ECMap
iter m = foldl go m . zip [1..]
  where
    go r (k, c) = case Map.lookup k r of
      Nothing -> Map.insert k (Map.singleton c 1) r
      Just x  -> Map.update (\_ -> Just (Map.insertWith (+) c 1 x)) k r

decode :: ECMap -> String
decode = Map.elems . Map.map (snd . head . sortBy (comparing Down)  . map swap . Map.assocs)

decode' :: ECMap -> String
decode' = Map.elems . Map.map (snd . head . sort . map swap . Map.assocs)

errorCorrected = decode . foldl iter Map.empty . lines
errorCorrected' = decode' . foldl iter Map.empty . lines

