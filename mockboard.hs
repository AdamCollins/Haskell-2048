module Mockboard (initialBoard, boardSize, Row, Tile) where

  type Board = [[Int]]
  type Row = [Int]
  type Tile = Int

  initialBoard :: Board
  initialBoard = 
    [[32,0,0,0],
     [0,0,64,0],
     [0,8,2,0],
     [0,0,4,2]]

  boardSize :: Float
  boardSize = foldl (\acc x -> 1 + acc) 0 initialBoard

  changedBoard = 
    [[0,2,0,0],
     [0,0,4,0],
     [0,8,0,0],
     [0,16,32,0]]