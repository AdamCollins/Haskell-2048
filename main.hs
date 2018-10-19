module Main (main) where
  import Control.Monad.Reader
  import Text.Read
  import System.Environment
  import Graphics.Gloss
  import Render
  import State

  -- Number of simulation steps to take for each second of real time
  simsteps = 1
  default_window_size = 580

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
    args <- getArgs
    let len = length args
    let command = args !! 0   -- this doesn't cause error on empty list, maybe because of lazy evaluation
    if len > 1 
      then putStrLn "too many arguments, try again"
      else
        if len == 0 
          then do 
            windowSize <- getUserInput
            runReader (playGame initialGame) windowSize
          else case command of
            "skip" -> runReader (playGame initialGame) default_window_size
            "win" -> runReader (playGame game1024) default_window_size
            -- TODO: add other cases
            "help" -> do 
              putStrLn "Command options:"
              putStrLn "-skip     skip the window size configs before playing"
              putStrLn "-win      load one step from win board"
              -- TODO: add more
            _ -> putStrLn "command not found, try again"

  playGame :: GameState -> Reader Float (IO ())
  playGame gameState = do
    window_size <- ask
    window <- windowIO
    return ( play window background simsteps gameState (drawing window_size) handleKeys (flip const) )