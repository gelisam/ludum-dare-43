-- Testing the AI module on a tiny game
{-# LANGUAGE LambdaCase, RankNTypes, TemplateHaskell #-}
module TicTacToe where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable

import AI
import AFold


makePrisms ''Bool


playerX, playerO :: Player
playerX = True
playerO = False

newtype Board = Board
  { unBoard :: [[Maybe Player]] }
  deriving (Eq, Show)

makePrisms ''Board

boardCells :: Traversal' Board (Maybe Player)
boardCells = _Board . each . each

boardLines :: [AFold Board (Maybe Player)]
boardLines = [AFold (_Board . ix i . each) | i <- [0..2]]
          ++ [AFold (_Board . each . ix i) | i <- [0..2]]
          ++ [mconcat [AFold (_Board . ix i . ix i    ) | i <- [0..2]]]
          ++ [mconcat [AFold (_Board . ix i . ix (2-i)) | i <- [0..2]]]

printBoard :: Board -> IO ()
printBoard (Board xss) = for_ xss $ \row -> do
  let row' = flip fmap row $ \case
        Nothing    -> '.'
        Just True  -> 'X'
        Just False -> 'O'
  putStrLn row'


initialBoard :: Board
initialBoard = Board . replicate 3 . replicate 3 $ Nothing

instance GameState Board where
  currentPlayer board = lengthOf (boardCells . _Just . _True ) board
                     <= lengthOf (boardCells . _Just . _False) board

  validMoves board = flip evalStateT False $ do
    let player = currentPlayer board
    board' <- forOf boardCells board $ \case
      Nothing -> do
        haveSwitched <- get
        if haveSwitched
        then pure Nothing
        else do
          shouldSwitch <- lift [True,False]
          if shouldSwitch
          then do
            put True
            pure (Just player)
          else
            pure Nothing
      Just contents -> pure (Just contents)
    haveSwitched <- get
    guard haveSwitched
    pure board'

  score board
    = sum [ [0,1,10,infinity] !! lengthOf (boardLine . _Just . _True ) board
          - [0,1,10,infinity] !! lengthOf (boardLine . _Just . _False) board
          | AFold boardLine <- boardLines
          ]
