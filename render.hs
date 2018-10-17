module Render (windowIO, background, drawing, handleKeys) where
  import Control.Monad.Reader
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game -- for Event
  import State

  squares_per_row = boardSize
  numof_edges = squares_per_row + 1
  square_to_edge_ratio = 7.6

  background :: Color
  background = makeColorI 187 173 160 255

  windowIO :: Reader Float Display
  windowIO = do
    size <- ask
    return ( InWindow "2048" (round size, round size) (10, 10) )

  edge_sizeIO :: Reader Float Float
  edge_sizeIO = do
    window_size <- ask
    return ( window_size / (numof_edges + squares_per_row * square_to_edge_ratio) )

  square_sizeIO :: Reader Float Float
  square_sizeIO = do
    edge_size <- edge_sizeIO
    return ( edge_size * square_to_edge_ratio )

  drawSquareIO :: Tile -> Index -> Reader Float Picture
  drawSquareIO tile index = do
    window_size <- ask
    edge_size <- edge_sizeIO
    square_size <- square_sizeIO
    return (
      translate (-1/2 * window_size + edge_size * (index + 1) + square_size * index + 1/2 * square_size) 0 $ 
      color (tileColor tile) $ rectangleSolid square_size square_size
      )

  drawRowIO :: Row -> Index -> Reader Float Picture
  drawRowIO row index = do
    window_size <- ask
    drawSquares <- sequence [drawSquareIO tile tindex | (tile, tindex) <- zip row [0..squares_per_row - 1]]
    edge_size <- edge_sizeIO
    square_size <- square_sizeIO
    return (translate 0 
                      (1/2 * window_size - edge_size * (index + 1) - square_size * index - 1/2 * square_size) 
                      $ pictures drawSquares)

  doDrawing :: GameState -> Reader Float Picture
  doDrawing gameState = do
    drawRows <- sequence [drawRowIO row index | (row, index) <- zip (board gameState) [0..squares_per_row - 1]]
    let gameOverDisplay = [drawGameOver | status gameState == Lose]
    let winDisplay = [drawWin | status gameState == Win]
    return ( pictures $ drawRows ++ gameOverDisplay ++ winDisplay )

  drawing :: Float -> GameState -> Picture
  drawing winsize gameState = 
    runReader (doDrawing gameState) winsize 

  drawGameOver :: Picture
  drawGameOver = rectangleSolid 300 300

  drawWin :: Picture
  drawWin = Circle 200

  
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