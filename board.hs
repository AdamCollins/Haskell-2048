module Board (insertTile, rotate, shiftRows, hasWon, hasLost) where
    import System.Random
    import System.IO
    import Data.List
    type Tile = Int
    type Row = [Tile]
    type Board = [[Tile]]


    initialBoard:: Board
    initialBoard = [[0, 0, 0, 0],
                 [0, 0, 0, 0],
                 [0, 0, 0, 0],
                 [0, 0, 0, 0]]

    seed = randomIO :: IO Int
     -- Inserts a new tile into the board at an 0 location
    insertTile :: Board -> Board
    insertTile [[]] = [[]]
    insertTile (r1:b) = (\(row,inserted) -> if inserted then (row:b) else (row : (insertTile b))) $ insertTileToRow r1

    -- Helper for insert
    insertTileToRow :: Row -> (Row, Bool)
    insertTileToRow [] = ([], False)
    insertTileToRow (h:t) = if(h==0) then ((2:t), True) else (\ (lst,bool) -> (h:lst,bool)) $ insertTileToRow t 

    flipdir dir 
                | dir=="left" = "right" 
                | dir=="right"="left" 
                | otherwise = dir  
    
    rotate :: [Char] -> Board -> Board
    rotate dir board 
                    | dir=="left" = reverse $ transpose board
                    | dir== "right" = transpose $ reverse board
                    | dir=="up" = rotate "left" (rotate "left" board)
                    | otherwise = board
    
    isRowEmpty :: Row -> Bool 
    isRowEmpty (h:t) = foldr (\ h b -> h==0&&b) True (h:t) 
    sumRows r1 r2 = ((zipWith (\ a b -> if (a==b || a*b==0) then a+b else b) r1 r2), (zipWith (\ a b -> if (a==b ||a*b==0) then 0 else a) r1 r2))
    --Returns true if the two rows can be added together legally
    rowsCanMerge :: Row->Row->Bool
    rowsCanMerge [] _ = True
    rowsCanMerge _ [] = True
    rowsCanMerge (e1:r1) (e2:r2) 
                            | (e1/=e2 || e1/=0 || e2/=0) = True
                            | otherwise = rowsCanMerge r1 r2
    
    shiftRows :: [Char]->Board->Board 
    shiftRows dir board = if (newBoard/=board) then insertTile newBoard else board
                where newBoard = rotate (flipdir dir) $ shiftRowDown (rotate dir board)


    --Shift all rows down while merging matching rows
    shiftRowDown :: Board -> Board                        
    shiftRowDown [] = []
    shiftRowDown (row : board)
                            | length board == 0 = [row]
                            | rowsCanMerge row (board!!0) = (snd mergedRows):(shiftRowDown ((fst mergedRows):boardEnd)) -- [0,0,0] : ([r1.a+r2.a, r1.b+r2.b...] :[r3])
                            | otherwise = row : shiftRowDown board
                            where emptyRow = [0,0,0,0]
                                  mergedRows = sumRows row (board!!0)
                                  boardEnd = drop 1 board
    --Returns true if the game has been won
    hasWon:: Board -> Bool 
    hasWon [] = False
    hasWon (r:b) = if (2048 `elem` r) then True else hasWon b 
   
    --Returns true if the game has been lost
    hasLost :: Board -> Bool
    hasLost board
                | board/=(shiftRows "left" board) = True
                | board/=(shiftRows "right" board) = True
                | board/=(shiftRows "up" board) = True
                | board/=(shiftRows "down" board) = True
                | otherwise = False

    --Prints Board in the console
    printBoard :: Show a => [a] -> IO ()
    printBoard b = mapM_ putStrLn $ (\ lst -> [show row | row<-lst]) b
