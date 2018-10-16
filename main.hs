import Graphics.Gloss
import Board
import Render
import State

mainstatic :: IO ()
mainstatic = display window background (drawing initialGame)

-- Number of simulation steps to take for each second of real time
simsteps = 1

main :: IO()
main = play window background simsteps initialGame drawing handleKeys (flip const)

-- main :: t0 -> IO()
-- main gameState = play window background simsteps gs drawing handleKeys (flip const)

-- maininit :: IO()
-- maininit = main initialGame