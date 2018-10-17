module Main (main) where
  import Control.Monad.Reader
  import Graphics.Gloss
  import Render
  import State

  -- mainstatic :: IO ()
  -- mainstatic = display window background (drawing initialGame)

  -- Number of simulation steps to take for each second of real time
  simsteps = 1

  main :: IO ()
  main = do
    putStrLn "Select your window size: 300 = small, 450 = medium, 600 = big"
    windowSize <- getLine
    runReader playGame (read windowSize :: Float)

  playGame :: Reader Float (IO ())
  playGame = do
    window_size <- ask
    window <- windowIO
    return ( play window background simsteps initialGame (drawing window_size) handleKeys (flip const) )