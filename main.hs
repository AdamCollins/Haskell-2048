module Main (main) where
  import Control.Monad.Reader
  import Text.Read
  import Graphics.Gloss
  import Render
  import State

  -- mainstatic :: IO ()
  -- mainstatic = display window background (drawing initialGame)

  -- Number of simulation steps to take for each second of real time
  simsteps = 1

  getUserInput :: IO Float
  getUserInput = do
    putStrLn "Select your window size between 100 and 800"
    windowSize <- getLine
    case readMaybe windowSize :: Maybe Float of
      Just x -> 
        if x > 200 && x < 800
          then return x 
          else putStrLn "Your input number is not in the valid range" >> getUserInput
      Nothing -> putStrLn "Invalid. Input must be a number" >> getUserInput

  main :: IO ()
  main = do
    windowSize <- getUserInput
    runReader playGame windowSize

  mainskip :: IO ()
  mainskip = do runReader playGame 580

  playGame :: Reader Float (IO ())
  playGame = do
    window_size <- ask
    window <- windowIO
    return ( play window background simsteps gameOverBoard (drawing window_size) handleKeys (flip const) )