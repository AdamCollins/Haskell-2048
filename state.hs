module State (Board, Row, Tile, Index,
              IsGameOver(..), GameState(..),
              boardSize, shiftUp, shiftDown, shiftLeft, shiftRight,

              initialBoard, board1024, gameOverboard,
              initialGame, game1024, gameover
             ) where

  import Board
  type Tile = Int
  type Row = [Tile]
  type Board = [[Tile]]
  type Index = Float
  data IsGameOver = InProgress | Win | Lose deriving (Eq, Show)
  data GameState = GameState {
    board :: Board,
    status :: IsGameOver
  }

  shiftUp :: GameState -> GameState
  shiftUp gs = GameState {
    board =  shiftRows "up" (board gs),
    status = getStatus $ shiftRows "up" (board gs)
  }
    
  shiftDown :: GameState -> GameState
  shiftDown gs = GameState {
    board = shiftRows "down" (board gs),
    status = getStatus $ shiftRows "down" (board gs)
  }

  shiftLeft :: GameState -> GameState
  shiftLeft gs = GameState {
    board = shiftRows "left" (board gs),
    status = getStatus $ shiftRows "left" (board gs)
  }

  shiftRight :: GameState -> GameState
  shiftRight gs = GameState {
    board = shiftRows "right" (board gs),
    status = getStatus $ shiftRows "right" (board gs)
  }

  getStatus board
               | hasWon board = Win
               | hasLost board = Lose
               | otherwise = InProgress

  -- we may also need for the demo
  -- a one step away from gameover board
  -- a one step away from win board
  -- a full board with many merges
  -- a board where certain directions will not cause any merge

  -- some boards
  initialBoard, board1024 :: Board
  initialGame, game1024 :: GameState

  initialGame = GameState {
    board = initialBoard,
    status = InProgress
  }
  initialBoard = [[2, 0, 0, 0],
                 [2, 2, 2, 0],
                 [0, 0, 4, 0],
                 [0, 0, 0, 0]]


  -- one step from winning the game
  game1024 = GameState {
    board = board1024,
    status = InProgress
  }
  board1024 =
    [[1024, 1024, 512, 128],
     [64, 32, 16, 8],
     [4, 2, 0, 0],
     [0, 0, 0, 0]]

  -- one step from losing the game
  gameover = GameState {
    board = gameOverboard,
    status = InProgress
  }
  gameOverboard = 
    [[2, 4, 2, 4],
     [8, 16, 8, 16],
     [16, 8, 16, 8],
     [2, 4, 2, 2]]

  boardSize :: Float
  boardSize = foldl (\acc x -> 1 + acc) 0 initialBoard
