{-# LANGUAGE ScopedTypeVariables #-}
module AI where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens hiding ((:<))


type Player = Bool

class GameState g where
  currentPlayer :: g -> Player
  validMoves    :: g -> [g]

  -- Bigger values mean the position looks good for player True.
  -- Use 'infinity' if True wins and 'negativeInfinity' if False wins.
  score :: g -> Double

infinity :: Double
infinity = 1/0

negativeInfinity :: Double
negativeInfinity = negate infinity


type GameTree = Cofree ZipList

gameTreeTop :: Lens' (GameTree a) a
gameTreeTop f (a :< xs) = (:< xs) <$> f a

gameTreeSub :: Lens' (GameTree a) (ZipList (GameTree a))
gameTreeSub f (a :< xs) = (a :<) <$> f xs

gameTreeFrom :: forall g. GameState g
             => g -> GameTree g
gameTreeFrom g = g :< ZipList subGameTrees
  where
    subGameTrees :: [GameTree g]
    subGameTrees = fmap gameTreeFrom (validMoves g)

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
        => g -> GameTree (ZipList Double)
minimax g = ZipList progressionOfEstimates :< ZipList subMinimaxes
  where
    baseValue :: Double
    baseValue = score g

    player :: Player
    player = currentPlayer g

    best :: [Double] -> Double
    best [] = baseValue
    best xs | player    = maximum xs
            | otherwise = minimum xs

    subMinimaxes :: [GameTree (ZipList Double)]
    subMinimaxes = fmap minimax (validMoves g)

    -- For each possible opponent move, an infinite list of estimates.
    subProgressionOfEstimates :: [ZipList Double]
    subProgressionOfEstimates = fmap (view gameTreeTop) subMinimaxes

    -- An infinite list, each element of which is set of estimates, one for each
    -- possible move by the opponent.
    progressionOfSubEstimates :: [[Double]]
    progressionOfSubEstimates = getZipList (sequenceA subProgressionOfEstimates)

    progressionOfEstimates :: [Double]
    progressionOfEstimates = baseValue : fmap best progressionOfSubEstimates
