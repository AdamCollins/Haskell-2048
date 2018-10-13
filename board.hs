module Board (initBoard, insert) where
    import System.IO
    type Tile = Int
    type Row = [Tile]
    type Board = [[Tile]]

    initBoard :: Board
    initBoard = [[2, 0, 0],
                [2, 0, 0],
                [0, 0, 0]]

     -- Inserts a new tile into the board at an 0 location
    insert :: Board -> Board
    insert [[]] = [[]]
    insert (r1:b) = (\(row,inserted) -> if inserted then (row:b) else (row : (insert b))) $ insertTileToRow r1

    -- Helper for insert
    insertTileToRow :: Row -> (Row, Bool)
    insertTileToRow [] = ([], False)
    insertTileToRow (h:t) = if(h==0) then ((2:t), True) else (\ (lst,bool) -> (h:lst,bool)) $ insertTileToRow t 
    
    --Prints Board in the console
    printBoard :: Show a => [a] -> IO ()
    printBoard b = mapM_ putStrLn $ (\ lst -> [show row | row<-lst]) b
