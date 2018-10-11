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
  color (tileColor tile) $ rectangleSolid square_size square_size

drawRow :: Row -> Float -> Picture
drawRow row index =
  translate 0 (1/2 * window_size - edge_size * (index + 1) - square_size * index - 1/2 * square_size) $ 
    pictures [drawSquare tile tindex | (tile, tindex) <- zip row [0..boardSize - 1]]

drawing :: Picture
drawing = pictures [drawRow row index | (row, index) <- zip initialBoard [0..boardSize - 1]]

main :: IO ()
main = display window background drawing

tileColor :: Tile -> Color
tileColor tile = case tile of
                   0     -> makeColorI 238 228 218 90
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