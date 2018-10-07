import System.IO

initBoard = [[0,0,0],
             [4,0,0],
             [0,2,0]]

strelems lst = [ (show x)++"" | x<-lst]
printBoard b = mapM_ putStrLn (strelems initBoard)

--play :: [[Num]] -> IO [[Num]]
play board = do
    putStrLn "Want to play 2048?"
    ans <- getLine
    if(ans `elem` ["yes", "y"])
        then 
            printBoard board
        else 
            return board

go = play ()