import System.IO

data Tile = Number Int | Empty deriving (Eq, Show)
type Board = [[Tile]]

initBoard :: Board
initBoard = [[Number 2, Empty, Empty],
             [Number 2, Empty, Empty],
             [Empty, Empty, Empty]]


insertTileToRow [] = ([], False)
insertTileToRow (h:t) = if(h==Empty) then ((Number 2:t), True) else (\ (lst,bool) -> (h:lst,bool)) $ insertTileToRow t

strelems lst = [show row | row<-lst]
printBoard b = mapM_ putStrLn (strelems b)

--play :: [[Num]] -> IO [[Num]]
-- play board = do
--     putStrLn "Want to play 2048?"
--     ans <- getLine
--     if(ans `elem` ["yes", "y"])
--         then 
--             printBoard initBoard
--         else 
--             return board
--
-- go = play ()