import Graphics.Gloss
import Mockboard

window_size :: Float
window_size = 296 * 2
squares_per_row = boardSize
numof_edges = squares_per_row + 1
square_to_edge_ratio = 8

edge_size = window_size / (numof_edges + squares_per_row * square_to_edge_ratio)
square_size = edge_size * square_to_edge_ratio

background :: Color
background = greyN 0.5

window :: Display
window = InWindow "Static 2048 Board" (round window_size, round window_size) (10, 10)

drawSquare :: Tile -> Float -> Picture
drawSquare tile index = 
  translate (-1/2 * window_size + edge_size * (index + 1) + square_size * index + 1/2 * square_size) 0 $ 
  color (greyN 0.7) $ rectangleSolid square_size square_size

drawRow :: Row -> Float -> Picture
drawRow row index =
  translate 0 (1/2 * window_size - edge_size * (index + 1) - square_size * index - 1/2 * square_size) $ 
    pictures [drawSquare tile tindex | tile <- row, tindex <- [0..boardSize - 1]]

drawing :: Picture
drawing = pictures [drawRow row index | row <- initialBoard, index <- [0..boardSize - 1]]

main :: IO ()
main = display window background drawing