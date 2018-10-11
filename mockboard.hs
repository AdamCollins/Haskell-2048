module Mockboard (initialBoard, boardSize, Row, Tile) where

  type Board = [[Int]]
  type Row = [Int]
  type Tile = Int

  initialBoard :: Board
  initialBoard = 
    [[0,0,0,0],
     [0,0,0,0],
     [0,0,2,0],
     [0,0,0,0]]

  boardSize :: Float
  boardSize = foldl (\acc x -> 1 + acc) 0 initialBoard

  changedBoard = 
    [[0,2,0,0],
     [0,0,4,0],
     [0,8,0,0],
     [0,16,32,0]]