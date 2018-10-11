import Graphics.Gloss

window_size :: Float
window_size = 296 * 2
squares_per_row = 4
numof_edges = squares_per_row + 1
square_to_edge_ratio = 8

edge_size = window_size / (numof_edges + squares_per_row * square_to_edge_ratio)
square_size = edge_size * square_to_edge_ratio

background :: Color
background = white

window :: Display
window = InWindow "Static 2048 Board" (round window_size, round window_size) (10, 10)

drawSquare :: Picture
drawSquare = translate (-1/2 * window_size + edge_size + 1/2 * square_size) (0) $ 
             color (dark blue) $ rectangleSolid square_size square_size

drawSquare2 :: Picture
drawSquare2 = translate (-1/2 * window_size + edge_size * 2 + square_size + 1/2 * square_size) 0 $ 
              color (dark yellow) $ rectangleSolid square_size square_size

drawSquare3 :: Picture
drawSquare3 = translate (1/2 * window_size - 2 * edge_size - square_size - 1/2 * square_size) 0 $ 
              color (dark red) $ rectangleSolid square_size square_size

drawSquare4 :: Picture
drawSquare4 = translate (1/2 * window_size - edge_size - 1/2 * square_size) 0 $ 
              color (dark green) $ rectangleSolid square_size square_size

drawing :: Picture
drawing = translate (0) (1/2 * window_size - edge_size - 1/2 * square_size) $ 
          pictures [drawSquare, drawSquare2, drawSquare3, drawSquare4]

main :: IO ()
main = display window background drawing