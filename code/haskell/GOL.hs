{-  
 Haskell Implementation of Game of Life
 Any live cell with fewer than two live neighbors dies, as if caused by under-population.
 Any live cell with two or three live neighbors lives on to the next generation.
 Any live cell with more than three live neighbors dies, as if by over-population..
 Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

TODO :- 
    1] Write a function for test cases (Input, Output)
    2] Commandline parameter passing 
    3] Run for x iterations
-}

import Data.List (unfoldr)
import Data.Array.Unboxed

-- utitily functions --

-- cls function clears the screen after each iteration --
cls :: IO ()
cls = putStrLn "\ESC[2J"

-- wait function waits for a certain amount of time after each iteration --
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

-- Board data type is basically an array of (x, y) bool where (x, y) represents map coordinates and if bool is true then the cell is alive if false then dead
type Board = UArray (Int,Int) Bool
 -- The grid is indexed by (y, x).
 
life :: Int -> Int -> Board -> Board
{- Returns the given Board advanced by one generation. 
   This is the main logic behind game -}
life w h old =
    listArray b (map f (range b)) -- using current board, and rules of gol advance by i generation and store it at ith place in an array 
  where b@((y1,x1),(y2,x2)) = bounds old
        f (y, x) = ( c && (n == 2 || n == 3) ) || ( not c && n == 3 ) -- check number of neighbors
          where c = get x y
                {- n = count number of neighbors that are alive where neighbors are at 
                      (x-1, y-1),
                      (x, y-1)
                      (x+1, y-1), 
                      (x-1, y)
                      (x+1, y)
                      (x-1, y+1)
                      (x, y+1)
                      (x-1, y+1) -}
                n = count [get (x + x') (y + y') | 
                    x' <- [-1, 0, 1], y' <- [-1, 0, 1],
                    not (x' == 0 && y' == 0)]
 
        get x y | x < x1 || x > x2 = False 
                | y < y1 || y > y2 = False
                | otherwise       = old ! (y, x)
 
 -- Counts number of cells that are true i.e. alive in an array --
count :: [Bool] -> Int
count = length . filter id

{- grid func converts user input into a 3-tuple of type (Int, Int, Board)
   where first and second members are width and height and last 
   member is the board defined with the position being true or 
   false. False if '.' and True if 'O'                         -}

grid :: [String] -> (Int, Int, Board)
grid l = (width, height, a)
  where (width, height) = (length $ head l, length l)
        a = listArray ((1, 1), (height, width)) $ concatMap f l
        f = map g
        g '.' = False
        g _   = True
 
-- printGrid func takes a board converts it into a map or a grid which is basically an array of arrays representing map, and then prints it line by line-- 
printGrid :: Int -> Board -> IO ()
printGrid width = mapM_ f . split width . elems
  where f = putStrLn . map g
        g False = '.'
        g _     = 'O'

-- splits a subarray from an array  --
split :: Int -> [a] -> [[a]]
split n = takeWhile (not . null) . unfoldr (Just . splitAt n)

glider = grid
   ["............",
    "............",
    "............",
    ".......OOO..",
    "............",
    "............",
    "............"]

-- this function runs the life uptil n generations and prints them one by one
gameOfLife :: Int -> (Int, Int, Board) -> IO ()
gameOfLife n (w, h, g) = mapM_ f $ take n $ iterate (life w h) g
  where f g = do
            cls
            putStrLn "------------------------------"
            printGrid w g
            wait 800000


main = do
       putStrLn "Enter number of generations you want to see: "
       numGen <- getLine
       gameOfLife (read numGen::Int) glider