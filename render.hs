import Graphics.Gloss

window_width, window_height :: Int
window_width = 296 * 2
window_height = window_width

background :: Color
background = white

window :: Display
window = InWindow "Static 2048 Board" (window_width, window_height) (10, 10)

drawSquare :: Picture
drawSquare = translate (-296 + 16 + 64) (0) $ color (dark blue) $ rectangleSolid 128 128

drawSquare2 :: Picture
drawSquare2 = translate (-296 + 16 + 128 + 16 + 64) 0 $ color (dark yellow) $ rectangleSolid 128 128

drawSquare3 :: Picture
drawSquare3 = translate (296 - 16 - 128 - 16 - 64) 0 $ color (dark red) $ rectangleSolid 128 128

drawSquare4 :: Picture
drawSquare4 = translate (296 - 16 - 64) 0 $ color (dark green) $ rectangleSolid 128 128

drawing :: Picture
drawing = pictures
  [drawSquare, drawSquare2, drawSquare3, drawSquare4]

main :: IO ()
main = display window background drawing