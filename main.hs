import System.IO
import System.Random
import Data.Time
import Data.Time.Clock.POSIX 

data Tile = Number Int | Empty deriving (Eq, Show)
type Board = [[Tile]]

initBoard :: Board
initBoard = [[Number 2, Empty, Empty],
             [Number 2, Empty, Empty],
             [Empty, Empty, Empty]]


-- Inserts a new tile into the board at an Empty location
insert :: Board -> Board
insert [[]] = [[]]
insert (r1:b) = (\(row,inserted) -> if inserted then (row:b) else (row : (insert b))) $ insertTileToRow r1
-- Helper for insert
insertTileToRow :: [Tile] -> ([Tile], Bool)
insertTileToRow [] = ([], False)
insertTileToRow (h:t) = if(h==Empty) then ((Number 2:t), True) else (\ (lst,bool) -> (h:lst,bool)) $ insertTileToRow t

--Prints Board in the console
printBoard :: Show a => [a] -> IO ()
printBoard b = mapM_ putStrLn (strelems b)
--Helper that converts rows to string
strelems lst = [show row | row<-lst]