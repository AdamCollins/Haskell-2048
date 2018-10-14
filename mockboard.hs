module Mockboard (initialBoard, board2048, changedBoard,
                  Row, Tile, Index) where

  type Tile = Int
  type Row = [Tile]
  type Board = [Row]
  type Index = Float

  initialBoard, board2048, changedBoard :: Board

  initialBoard = 
    [[32,0,0,0],
     [0,0,64,0],
     [0,8,2,0],
     [0,0,4,2]]

  board2048 =
    [[2048, 1024, 512, 128],
     [64, 32, 16, 8],
     [4, 2, 0, 0],
     [0, 0, 0, 0]]

  changedBoard = 
    [[0,2,0,0],
     [0,0,4,0],
     [0,8,0,0],
     [0,16,32,0]]