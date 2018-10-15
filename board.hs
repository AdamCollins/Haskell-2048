module Board (initBoard, insertTile, rotate) where
    import System.IO
    import Data.List
    type Tile = Int
    type Row = [Tile]
    type Board = [[Tile]]

    initBoard :: Board
    initBoard = [[2, 0, 0],
                [2, 0, 0],
                [0, 0, 0]]

     -- Inserts a new tile into the board at an 0 location
    insertTile :: Board -> Board
    insertTile [[]] = [[]]
    insertTile (r1:b) = (\(row,inserted) -> if inserted then (row:b) else (row : (insertTile b))) $ insertTileToRow r1

    -- Helper for insert
    insertTileToRow :: Row -> (Row, Bool)
    insertTileToRow [] = ([], False)
    insertTileToRow (h:t) = if(h==0) then ((2:t), True) else (\ (lst,bool) -> (h:lst,bool)) $ insertTileToRow t 
    
    rotate :: [Char] -> Board -> Board  
    rotate dir board 
                    | dir=="left" = reverse $ transpose board
                    | dir== "right" = transpose $ reverse board
                    | dir=="up" = rotate "left" (rotate "left" board)
                    | otherwise = board
    
    isRowEmpty :: Row -> Bool 
    isRowEmpty (h:t) = foldr (\ h b -> h==0&&b) True (h:t) 


    --Prints Board in the console
    printBoard :: Show a => [a] -> IO ()
    printBoard b = mapM_ putStrLn $ (\ lst -> [show row | row<-lst]) b
