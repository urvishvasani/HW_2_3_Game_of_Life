# Julia Implementation of Game of Life
# Any live cell with fewer than two live neighbors dies, as if caused by under-population.
# Any live cell with two or three live neighbors lives on to the next generation.
# Any live cell with more than three live neighbors dies, as if by over-population..
# Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

#TODO :- 
#    1] Write a function for test cases (Input, Output)
#    2] Commandline parameter passing 
#    3] Run for x iterations

"""
function count_alive :- Calculates the number of alive neighbours present for a cell(x,y) on the board
Input:
    board :- 2D Grid/board
    row :- x coordinate of the current cell
    column :- y coordinate ofthe current cell

Output:
    count :- Total number of alive neighbours of the current cell
"""
function count_alive(board,row,column)
    check = [[0 1], [1 0], [1 1], [0 -1], [-1 0], [-1 -1], [1 -1], [-1 1]]
    count = 0
    for c in check
        if (row + c[1]) <= 0 || (column + c[2]) <= 0 || (row + c[1]) > size(board)[1] || (column + c[2]) > size(board[1])[1]
            continue
        end
        if board[row + c[1]][column + c[2]] == 1
            count += 1
        end
    end
    return count
end


"""
function gameOfLife :- Generates the next cell structure for the grid
Input:
    board :- Input 2D Grid/board
Output:
    board :- Next generated cell structure according to the rules
"""
function gameOfLife(board)
    rows = size(board)
    column = size(board[1])
    changed = []
    for i in 1:rows[1]
        for j in 1:column[1]
            neighbours = count_alive(board, i, j)
            println(neighbours," ",i,j)
            if board[i][j] == 1
                if neighbours < 2 || neighbours>3
                    append!(changed,[[i,j]])
                end
            else
                if neighbours==3
                    append!(changed,[[i,j]])
                end
            end
        end
    end
    
    for toggle_index in changed
        x = toggle_index[1]
        y = toggle_index[2]
        board[x][y]  = 1 - board[x][y]
    end
    return board
end

#function run_test_cases()
print(gameOfLife([[0,1,0],[0,0,1],[1,1,1],[0,0,0]]))


