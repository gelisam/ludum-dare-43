module Types where

import AI


type HP   = Int
type Mana = Int

data Card
  = CheapCard
  | MediumCard
  | ExpensiveCard

data Side = Side
  { drawPile    :: [Card]
  , discardPile :: [Card]
  , hand        :: [Card]
  , activeArea  :: [(Card, HP)]
  , hp          :: HP
  }

data Board = Board
  { currentPlayer :: Player
  , mySide        :: Side
  , opponentSide  :: Side
  }

--instance GameState Board where
--  type Move Board = Card
--  
--  currentPlayer :: Board -> Player
--  currentPlayer = undefined
--
--  validMoves :: Board -> Map Card Board
--  validMoves = undefined
--
--  -- Bigger values mean the position looks good for player True.
--  -- Use 'infinity' if True wins and 'negativeInfinity' if False wins.
--  score :: Board -> Double
--  score = undefined

maxHp :: Card -> HP
maxHp = undefined

manaCost :: Card -> Mana
manaCost = undefined

attackPower :: Card -> HP
attackPower = undefined

counterAttackPower :: Card -> HP
counterAttackPower = undefined
