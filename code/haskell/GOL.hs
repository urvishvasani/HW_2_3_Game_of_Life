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

-- Board data type is basically an array of (x, y) bool where (x, y) represents map coordinates and if bool is true then the cell is alive if false then dead
type Board = UArray (Int,Int) Bool
 -- The grid is indexed by (y, x).
 
life :: Int -> Int -> Board -> Board
{- Returns the given Board advanced by one generation. 
   This is the main logic behind game -}
life w h old =
    listArray b (map f (range b)) -- using current board, and rules of gol advance by i generation and store it at ith place in an array 
  where b@((y1,x1),(y2,x2)) = bounds old
        f (x, y) = ( c && (n == 2 || n == 3) ) || ( not c && n == 3 ) -- check number of neighbors
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
 
-- printGrid func takes a board converts it into a map or a grid which is basically an array of arrays representing map, and then prints it line by line -- 
printGrid :: Int -> Board -> IO ()
printGrid width = mapM_ f . split width . elems
  where f = putStrLn . map g
        g False = '.'
        g _     = 'O'

-- this function gets two array of boards where the board at index i in the array is ith generation of that test case, the other board is ground truth, it checks if both are equal --
test :: [Board] -> [Board] -> IO ()
test b1 b2 | b1 /= b2 = putStrLn "Test case failed"
           | b1 == b2 = putStrLn "Test case passed"

-- runTest function takes true life and a grid, it generates output for grid using life function and runs test on it --
runTest :: Int -> (Int, Int, Board) -> [Board] -> IO ()
runTest n (w1, h1, g1) truth = test life1 truth
                                      where 
                                      	life1 = take n $ iterate life w1 h1 g1
                                        

-- splits a subarray from an array  --
split :: Int -> [a] -> [[a]]
split n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- Test and Truth definitions --
test0 = grid
   [".0.",
    "..0",
    "000",
    "..."]

w0 = 3 :: Int
truth0 = [array ((1,1),(4,3)) [((1,1),False),((1,2),True),((1,3),False),((2,1),False),((2,2),False),((2,3),True),((3,1),True),((3,2),True),((3,3),True),((4,1),False),((4,2),False),((4,3),False)],array ((1,1),(4,3)) [((1,1),False),((1,2),False),((1,3),False),((2,1),True),((2,2),False),((2,3),True),((3,1),False),((3,2),True),((3,3),True),((4,1),False),((4,2),True),((4,3),False)],array ((1,1),(4,3)) [((1,1),False),((1,2),False),((1,3),False),((2,1),False),((2,2),False),((2,3),True),((3,1),True),((3,2),False),((3,3),True),((4,1),False),((4,2),True),((4,3),True)]]

test1 = grid
   ["00.0.",
    "...0",
    "0.00",
    ".0.."]

w1 = 5 :: Int
truth1 = [array ((1,1),(4,5)) [((1,1),True),((1,2),True),((1,3),False),((1,4),True),((1,5),False),((2,1),False),((2,2),False),((2,3),False),((2,4),True),((2,5),True),((3,1),False),((3,2),True),((3,3),True),((3,4),False),((3,5),True),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),False)],array ((1,1),(4,5)) [((1,1),False),((1,2),False),((1,3),True),((1,4),True),((1,5),True),((2,1),True),((2,2),False),((2,3),False),((2,4),False),((2,5),True),((3,1),False),((3,2),False),((3,3),True),((3,4),False),((3,5),True),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),False)],array ((1,1),(4,5)) [((1,1),False),((1,2),False),((1,3),False),((1,4),True),((1,5),True),((2,1),False),((2,2),True),((2,3),True),((2,4),False),((2,5),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((3,5),False),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),False)]]

test2 = grid
   ["0..0",
    "00.0",
    "0..0",
    ".00."]

w2 = 4 :: Int
truth2 = [array ((1,1),(4,4)) [((1,1),True),((1,2),False),((1,3),False),((1,4),True),((2,1),True),((2,2),True),((2,3),False),((2,4),True),((3,1),True),((3,2),False),((3,3),False),((3,4),True),((4,1),False),((4,2),True),((4,3),True),((4,4),False)],array ((1,1),(4,4)) [((1,1),True),((1,2),True),((1,3),True),((1,4),False),((2,1),True),((2,2),True),((2,3),False),((2,4),True),((3,1),True),((3,2),False),((3,3),False),((3,4),True),((4,1),False),((4,2),True),((4,3),True),((4,4),False)],array ((1,1),(4,4)) [((1,1),True),((1,2),False),((1,3),True),((1,4),False),((2,1),False),((2,2),False),((2,3),False),((2,4),True),((3,1),True),((3,2),False),((3,3),False),((3,4),True),((4,1),False),((4,2),True),((4,3),True),((4,4),False)]]
    
test3 = grid
   ["0..",
    "..0",
    "0..",
    "..0"]

w3 = 3 :: Int
truth3 = [array ((1,1),(4,3)) [((1,1),True),((1,2),False),((1,3),False),((2,1),False),((2,2),False),((2,3),True),((3,1),True),((3,2),False),((3,3),False),((4,1),False),((4,2),False),((4,3),True)],array ((1,1),(4,3)) [((1,1),False),((1,2),False),((1,3),False),((2,1),False),((2,2),True),((2,3),False),((3,1),False),((3,2),True),((3,3),False),((4,1),False),((4,2),False),((4,3),False)],array ((1,1),(4,3)) [((1,1),False),((1,2),False),((1,3),False),((2,1),False),((2,2),False),((2,3),False),((3,1),False),((3,2),False),((3,3),False),((4,1),False),((4,2),False),((4,3),False)]]
    
test4 = grid
   ["00...",
    "....0",
    "...00",
    "....."]  

w4 = 5 :: Int
truth4 = [array ((1,1),(4,5)) [((1,1),True),((1,2),True),((1,3),False),((1,4),False),((1,5),False),((2,1),False),((2,2),False),((2,3),False),((2,4),False),((2,5),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((3,5),True),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),False)],array ((1,1),(4,5)) [((1,1),False),((1,2),False),((1,3),False),((1,4),False),((1,5),False),((2,1),False),((2,2),False),((2,3),False),((2,4),True),((2,5),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((3,5),True),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),False)],array ((1,1),(4,5)) [((1,1),False),((1,2),False),((1,3),False),((1,4),False),((1,5),False),((2,1),False),((2,2),False),((2,3),False),((2,4),True),((2,5),True),((3,1),False),((3,2),False),((3,3),False),((3,4),True),((3,5),True),((4,1),False),((4,2),False),((4,3),False),((4,4),False),((4,5),False)]]

-- this function prints the original truth
printTruth :: Int -> [Board] -> IO ()
printTruth w gTruth = mapM_ f gTruth
  where f t = do
            putStrLn "x-x-x-x-x-x-x"
            printGrid w t

-- this function generates n generations and prints them one by one
gameOfLife :: Int -> (Int, Int, Board) -> IO ()
gameOfLife n (w, h, g) = mapM_ f $ take n $ iterate life w h g
  where f g = do
            putStrLn "+-+-+-+-+-+-+"
            printGrid w g


main = do

       putStrLn "--------------Test 0---------------"
       
       gameOfLife 3 test0
       
       putStrLn "--------Truth--------"
       printTruth w0 truth0
       runTest 3 test0 truth0

       putStrLn "--------------Test 1---------------"
       
       gameOfLife 3 test1

       putStrLn "--------Truth--------"
       printTruth w1 truth1
       runTest 3 test1 truth1

       putStrLn "--------------Test 2---------------"
       
       gameOfLife 3 test2
       
       putStrLn "--------Truth--------"
       printTruth w2 truth2
       runTest 3 test2 truth2

       putStrLn "--------------Test 3---------------"
       
       gameOfLife 3 test3
       
       putStrLn "--------Truth--------"
       printTruth w3 truth3
       runTest 3 test3 truth3

       putStrLn "--------------Test 4---------------"
       
       gameOfLife 3 test4

       putStrLn "--------Truth--------"
       printTruth w4 truth4
       runTest 3 test4 truth4
