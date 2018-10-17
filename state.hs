module State (Board, Row, Tile, Index,
              IsGameOver(..), GameState(..),
              boardSize, shiftUp, shiftDown, shiftLeft, shiftRight,

              initialBoard, board2048, changedBoard, changedBoard2, gameOverBoard, winBoard,
              initialGame, game2048, gameState1, gameState2,
             ) where

  import Board

  data IsGameOver = InProgress | Win | Lose deriving (Eq)
  data GameState = GameState {
    board :: Board,
    status :: IsGameOver
  }

  shiftUp :: GameState -> GameState
  shiftUp gs = GameState {
    board = shiftRows "up" (board gs),
    status = InProgress
  }
    
  shiftDown :: GameState -> GameState
  shiftDown gs = GameState {
    board = shiftRows "down" (board gs),
    status = InProgress
  }

  shiftLeft :: GameState -> GameState
  shiftLeft gs = GameState {
    board = shiftRows "left" (board gs),
    status = InProgress
  }

  shiftRight :: GameState -> GameState
  shiftRight gs = GameState {
    board = shiftRows "right" (board gs),
    status = InProgress
  }


  -- we may also need for the demo
  -- a one step away from gameover board
  -- a one step away from win board
  -- a full board with many merges
  -- a board where certain directions will not cause any merge

  -- some boards
  initialBoard, board2048, changedBoard :: Board
  initialGame, game2048, gameState1 :: GameState

  initialGame = GameState {
    board = initialBoard,
    status = InProgress
  }
  initialBoard = [[2, 0, 0, 0],
                 [2, 2, 2, 0],
                 [0, 0, 4, 0],
                 [0, 0, 0, 0]]


  game2048 = GameState {
    board = board2048,
    status = Win
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
    [[2, 2, 2, 2],
     [4, 4, 4, 4],
     [0, 0, 0, 0],
     [0, 0, 0, 0]]


  gameState2 = GameState {
      board = changedBoard2,
      status = InProgress
  }
  changedBoard2 = 
    [[0, 0, 0, 0],
     [0, 0, 0, 0],
     [64, 64, 64, 64],
     [32, 32, 32, 32]]


  gameOverBoard = GameState {
      board = initialBoard,
      status = Lose
  }

  winBoard = GameState {
    board = board2048,
    status = Win
  }

  boardSize :: Float
  boardSize = foldl (\acc x -> 1 + acc) 0 initialBoard
