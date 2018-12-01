-- Testing the AI module on a tiny game
{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TemplateHaskell, TypeFamilies #-}
module TicTacToe where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import Data.Foldable
import qualified Data.Set as Set

import AI
import AFold


makePrisms ''Bool


playerX, playerO :: Player
playerX = True
playerO = False


type Pos = (Int, Int)

boardPositions :: [Pos]
boardPositions = [(i, j) | i <- [0..2], j <- [0..2]]


newtype Board = Board
  { unBoard :: Array Pos (Maybe Player) }
  deriving (Eq, Show)

makePrisms ''Board

initialBoard :: Board
initialBoard = Board . listArray ((0,0),(2,2)) . replicate 9 $ Nothing

boardLines :: [AFold Board (Maybe Player)]
boardLines = [mconcat [AFold (_Board . ix (i,   j)) | i <- [0..2]] | j <- [0..2]]
          ++ [mconcat [AFold (_Board . ix (i,   j)) | j <- [0..2]] | i <- [0..2]]
          ++ [mconcat [AFold (_Board . ix (i,   i)) | i <- [0..2]]]
          ++ [mconcat [AFold (_Board . ix (i, 2-i)) | i <- [0..2]]]

printBoard :: Board -> IO ()
printBoard (Board xss) = do
  for_ [0..2] $ \j -> do
    for_ [0..2] $ \i -> do
      case xss ! (i,j) of
        Nothing    -> putStr "."
        Just True  -> putStr "X"
        Just False -> putStr "O"
    putStrLn ""


instance GameState Board where
  type Move Board = Pos

  currentPlayer board = lengthOf (_Board . each . _Just . _True ) board
                     <= lengthOf (_Board . each . _Just . _False) board

  validMoves = Set.fromList
             . findIndicesOf (_Board . ifolded) (== Nothing)

  playMove pos g = set (_Board . ix pos) (Just (currentPlayer g)) g

  score board
    = sum [ [0,1,10,infinity] !! lengthOf (boardLine . _Just . _True ) board
          - [0,1,10,infinity] !! lengthOf (boardLine . _Just . _False) board
          | AFold boardLine <- boardLines
          ]
