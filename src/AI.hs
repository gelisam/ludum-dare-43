{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables #-}
module AI where

import Control.Applicative
import Control.Lens hiding ((:<))
import Data.Foldable
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map

import GameTree


type Player = Bool

class Ord (Move g) => GameState g where
  type Move g
  currentPlayer :: g -> Player
  validMoves    :: g -> Set (Move g)
  playMove      :: Move g -> g -> g

  -- Bigger values mean the position looks good for player True.
  -- Use 'infinity' if True wins and 'negativeInfinity' if False wins.
  score :: g -> Double

moveMap :: GameState g
        => g -> Map (Move g) g
moveMap g = Map.fromList
          [ (move, playMove move g)
          | move <- toList (validMoves g)
          ]

infinity :: Double
infinity = 1/0

negativeInfinity :: Double
negativeInfinity = negate infinity


gameTreeFrom :: forall g. GameState g
             => g -> GameTree (Move g) g
gameTreeFrom g = g :< subGameTrees
  where
    subGameTrees :: Map (Move g) (GameTree (Move g) g)
    subGameTrees = gameTreeFrom <$> moveMap g

-- Each game state is annotated with an infinite sequence of increasingly-better
-- estimates, starting with 'score'.
--
-- If you call 'minimax' on each turn with the current game state, the
-- computation will start from scratch each time. But If you call it once at the
-- beginning and then dive deeper and deeper into the resulting GameTree, the AI
-- will be able to keep some of its previously-computed results.
-- 
-- If you don't want the AI to cheat by looking at your cards, don't give it
-- that information! Model the game asymmetrically, with the AI's hand being
-- represented by a list of cards, and the opponent's hand being represented by
-- an integer representing the number of cards.
minimax :: forall g. GameState g
        => g -> GameTree (Move g) (ZipList Double)
minimax g = ZipList progressionOfEstimates :< subMinimaxes
  where
    baseValue :: Double
    baseValue = score g

    player :: Player
    player = currentPlayer g

    best :: [Double] -> Double
    best [] = baseValue
    best xs | player    = maximum xs
            | otherwise = minimum xs

    subMinimaxes :: Map (Move g) (GameTree (Move g) (ZipList Double))
    subMinimaxes = minimax <$> moveMap g

    -- For each possible opponent move, an infinite list of estimates.
    subProgressionOfEstimates :: Map (Move g) (ZipList Double)
    subProgressionOfEstimates = view gameTreeTop <$> subMinimaxes

    -- An infinite list, each element of which is set of estimates, one for each
    -- possible move by the opponent.
    progressionOfSubEstimates :: [Map (Move g) Double]
    progressionOfSubEstimates = getZipList (sequenceA subProgressionOfEstimates)

    progressionOfEstimates :: [Double]
    progressionOfEstimates = baseValue : fmap (best . toList) progressionOfSubEstimates
