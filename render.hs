module Render (window, background, drawing, handleKeys) where
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game -- for Event
  import State

  window_size :: Float
  window_size = 296 * 2
  squares_per_row = boardSize
  numof_edges = squares_per_row + 1
  square_to_edge_ratio = 7.6

  edge_size = window_size / (numof_edges + squares_per_row * square_to_edge_ratio)
  square_size = edge_size * square_to_edge_ratio

  background :: Color
  background = makeColorI 187 173 160 255

  window :: Display
  window = InWindow "2048" (round window_size, round window_size) (10, 10)

  drawSquare :: Tile -> Index -> Picture
  drawSquare tile index = 
    translate (-1/2 * window_size + edge_size * (index + 1) + square_size * index + 1/2 * square_size) 0 $ 
    color (tileColor tile) $ rectangleSolid square_size square_size

  drawRow :: Row -> Index -> Picture
  drawRow row index =
    translate 0 (1/2 * window_size - edge_size * (index + 1) - square_size * index - 1/2 * square_size) $ 
      pictures [drawSquare tile tindex | (tile, tindex) <- zip row [0..squares_per_row - 1]]

  drawGameOver :: Picture
  drawGameOver = rectangleSolid 300 300

  drawWin :: Picture
  drawWin = Circle 200

  drawing :: GameState -> Picture
  drawing gameState = pictures $ 
                      [drawRow row index | (row, index) <- zip (board gameState) [0..squares_per_row - 1]]
                      ++ gameOverDisplay ++ winDisplay
    where gameOverDisplay = [drawGameOver | status gameState == Lose]
          winDisplay = [drawWin | status gameState == Win]

  handleKeys :: Event -> GameState -> GameState
  handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs = shiftLeft gs
  handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs = shiftRight gs
  handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gs = shiftUp gs
  handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gs = shiftDown gs
  -- a special key to test win/lose display; to be deleted
  handleKeys (EventKey (Char 'a') Down _ _) s = winBoard
  handleKeys _ gs = gs

  tileColor :: Tile -> Color
  tileColor tile = case tile of
                    0     -> makeColorI 205 192 180 255
                    2     -> makeColorI 238 228 218 255
                    4     -> makeColorI 237 224 200 255
                    8     -> makeColorI 242 177 121 255
                    16    -> makeColorI 245 149 99 255
                    32    -> makeColorI 246 124 95 255
                    64    -> makeColorI 246 94 59 255
                    128   -> makeColorI 237 207 114 255
                    256   -> makeColorI 237 204 97 255
                    512   -> makeColorI 237 200 80 255
                    1024  -> makeColorI 237 197 63 255
                    2048  -> makeColorI 237 194 46 255
                    _     -> makeColorI 238 228 218 90