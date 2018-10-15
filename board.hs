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

    sumRows r1 r2 = zipWith (+) r1 r2
    --Returns true if the two rows can be added together legally
    rowsCanMerge [] [] = True
    rowsCanMerge (e1:r1) (e2:r2) 
                            | (e1==e2 || e1==0 || e2==0) = rowsCanMerge r1 r2
                            | otherwise = False

    --Shift all rows down while merging matching rows
    shiftRowDown :: Board -> Board                        
    shiftRowDown [] = []
    shiftRowDown (row : board)
                            | length board == 0 = [row]
                            | rowsCanMerge row (board!!0) = emptyRow:(shiftRowDown (mergedRow:boardEnd)) -- [0,0,0] : ([r1.a+r2.a, r1.b+r2.b...] :[r3])
                            | otherwise = row : shiftRowDown board
                            where emptyRow = [0,0,0]
                                  mergedRow = sumRows row (board!!0)
                                  boardEnd = drop 1 board


    --Prints Board in the console
    printBoard :: Show a => [a] -> IO ()
    printBoard b = mapM_ putStrLn $ (\ lst -> [show row | row<-lst]) b
