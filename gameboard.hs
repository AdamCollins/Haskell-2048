module GameBoard (initialBoard, board2048, changedBoard, changedBoard2, gameOverBoard,
                  initialGame, game2048, gameState1, gameState2,
                  Board, Row, Tile, Index,
                  boardSize,
                  IsGameOver(..), GameState(..)
                  ) where

  type Tile = Int
  type Row = [Tile]
  type Board = [Row]
  type Index = Float

  data IsGameOver = InProgress | GameOver deriving (Eq)
  data GameState = GameState {
    board :: Board,
    status :: IsGameOver
  }

  -- we may also need for the demo
  -- a one step away from gameover board
  -- a one step away from win board
  -- a full board with many merges
  -- a board where certain directions will not cause any merge
  initialBoard, board2048, changedBoard :: Board
  initialGame, game2048, gameState1 :: GameState

  initialGame = GameState {
    board = initialBoard,
    status = InProgress
  }
  initialBoard = 
    [[32,0,0,0],
     [0,0,64,0],
     [0,8,2,0],
     [0,0,4,2]]


  game2048 = GameState {
    board = board2048,
    status = InProgress
  }
  board2048 =
    [[2048, 1024, 512, 128],
     [64, 32, 16, 8],
     [4, 2, 0, 0],
     [0, 0, 0, 0]]


  gameState1 = GameState {
      board = changedBoard,
      status = InProgress
  }
  changedBoard = 
    [[2,2,2,2],
     [4,4,4,4],
     [0,0,0,0],
     [0,0,0,0]]


  gameState2 = GameState {
      board = changedBoard2,
      status = InProgress
  }
  changedBoard2 = 
    [[0,0,0,0],
     [0,0,0,0],
     [64,64,64,64],
     [32,32,32,32]]


  gameOverBoard = GameState {
      board = initialBoard,
      status = GameOver
  }

  boardSize :: Float
  boardSize = foldl (\acc x -> 1 + acc) 0 initialBoard