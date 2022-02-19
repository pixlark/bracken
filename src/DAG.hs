{-# LANGUAGE NamedFieldPuns #-}

module DAG where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Debug.Trace

--
-- Simple implementation of a Directed Acyclic Graph, which can be reduced to a topological ordering
--

type NodeSet a = Set.Set a
type EdgeMap a = Map.Map a [a]

--newtype DAG a = DAG (EdgeMap a)
--  deriving (Show)
data DAG a = DAG { edgeMap  :: EdgeMap a
                 , allNodes :: NodeSet a }

empty = DAG { edgeMap = Map.empty, allNodes = Set.empty }

insert :: (Eq a, Ord a, Show a) => a -> DAG a -> DAG a
insert x (DAG { edgeMap, allNodes }) = DAG { edgeMap  = Map.insert x [] edgeMap
                                           , allNodes = Set.insert x allNodes }

depends :: (Eq a, Ord a, Show a) => (a, a) -> DAG a -> DAG a
depends (x, y) (DAG { edgeMap, allNodes })
  = DAG { edgeMap  = Map.insertWith (++) x [y]
                   $ Map.insertWith (const id) y [] edgeMap
        , allNodes = Set.insert x $ Set.insert y allNodes }

fromNodes :: (Eq a, Ord a, Show a) => [a] -> DAG a
fromNodes nodes = DAG { edgeMap = Map.empty
                      , allNodes = Set.fromList nodes }

fromList :: (Eq a, Ord a, Show a) => [(a, a)] -> DAG a
fromList = foldl' (flip depends) empty

findSink :: (Eq a, Ord a, Show a) => DAG a -> Maybe a
findSink (DAG { edgeMap })
  = dfs Set.empty $ fst $ head $ Map.assocs edgeMap
  where dfs visited x
          | x `Set.member` visited = Nothing
          | otherwise = let connected = (Map.!) edgeMap x
                        in if length connected == 0
                           then Just x else dfs (Set.insert x visited) $ head connected

remove :: (Eq a, Ord a, Show a) => a -> DAG a -> DAG a
remove x (DAG { edgeMap, allNodes })
  = DAG { edgeMap  = Map.map (filter (/= x)) $ Map.delete x edgeMap
        , allNodes = Set.delete x allNodes }

order :: (Eq a, Ord a, Show a) => DAG a -> Maybe [a]
order dag@(DAG { edgeMap, allNodes })
  | Map.size edgeMap == 0 = Just $ Set.elems allNodes
  | otherwise = do sink <- findSink dag
                   let newDag = remove sink dag
                   rest <- order newDag
                   return $ sink:rest
