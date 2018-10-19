-- The end of game images(cross/circle) are taken from this game:
-- https://gist.github.com/gallais/0d61677fe97aa01a12d5

module Render (windowIO, background, drawing, handleKeys) where
  import Control.Monad.Reader
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game -- for Event
  import State

  squares_per_row = boardSize
  numof_edges = squares_per_row + 1
  square_to_edge_ratio = 7.6
  scale_factor = 2600

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

  posToLeft :: String -> Float
  posToLeft tile = case move of
                    4 -> 3.8
                    3 -> 5
                    2 -> 8
                    1 -> 15
                  where move = foldl (\acc x -> 1 + acc) 0 tile

  tileValueIO :: Tile -> Reader Float [Picture]
  tileValueIO tile = do
    window_size <- ask
    square_size <- square_sizeIO
    let amount = window_size / scale_factor
    let posx = posToLeft $ show tile
    let tileValue = if tile == 0 
                    then [] 
                    else [translate (- square_size / posx) (- square_size / 10) $ 
                          Scale amount amount $ color black $ text $ show tile]
    return tileValue

  drawSquareIO :: Tile -> Index -> Reader Float Picture
  drawSquareIO tile index = do
    window_size <- ask
    edge_size <- edge_sizeIO
    square_size <- square_sizeIO
    tileValue <- tileValueIO tile
    return (
      translate (-1/2 * window_size + edge_size * (index + 1) + square_size * index + 1/2 * square_size) 0 $ 
      color (tileColor tile) $ pictures $ [rectangleSolid square_size square_size] ++ tileValue
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
    winsize <- ask
    drawRows <- sequence [drawRowIO row index | (row, index) <- zip (board gameState) [0..squares_per_row - 1]]
    let gameOverDisplay = [drawGameOver winsize | status gameState == Lose]
    let winDisplay = [drawWin winsize | status gameState == Win]
    return ( pictures $ drawRows ++ gameOverDisplay ++ winDisplay )

  drawing :: Float -> GameState -> Picture
  drawing winsize gameState = 
    runReader (doDrawing gameState) winsize 

  resize :: Float -> Path -> Path
  resize k = fmap (\ (x, y) -> (x * k, y * k))

  drawWin :: Float -> Picture
  drawWin winsize = color green $ translate 0 0 $ thickCircle (0.1 * k) (0.3 * k)
                         where k = winsize / 1.25

  drawGameOver :: Float -> Picture
  drawGameOver winsize = color red $ translate 0 0 $ pictures $ fmap (polygon . resize (winsize / 1.6))
   [ [ (-0.35, -0.25), (-0.25, -0.35), (0.35,0.25), (0.25, 0.35) ]
   , [ (0.35, -0.25), (0.25, -0.35), (-0.35,0.25), (-0.25, 0.35) ]
   ]

  
  handleKeys :: Event -> GameState -> GameState
  handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs = shiftLeft gs
  handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs = shiftRight gs
  handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gs = shiftUp gs
  handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gs = shiftDown gs
  -- a special key to test win/lose display; to be deleted
  -- handleKeys (EventKey (Char 'a') Down _ _) s = winBoard
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