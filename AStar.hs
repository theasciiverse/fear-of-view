module AStar where

import           Data.Graph.AStar

import           Data.Hashable

import qualified Data.HashSet     as HS

-- find all min-length paths
aStarAll :: (Hashable a, Ord a, Ord c, Num c) =>
         (a -> HS.HashSet a) -> (a -> c) -> (a -> Bool) -> a -> [[a]]
aStarAll graph heur goal start = go Nothing HS.empty where
    go optLen exclude = case aStar graph' dist heur goal start of
            Just path@(first:_) | maybe True (length path ==) optLen -> path : go (Just $ length path) (HS.insert first exclude)
            _ -> []
        where graph' v = graph v `HS.difference` exclude
    dist = const $ const 1
