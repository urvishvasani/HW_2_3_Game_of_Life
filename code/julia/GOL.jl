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
    check = [[0 1], [1 0], [1 1], [0 -1], [-1 0], [1 -1], [-1 1],[-1,-1]]
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
function gameOfLife(board,generations)
    new_board = deepcopy(board)
    rows = size(board)
    column = size(board[1])
    for generation in 1:generations
        changed = []
        for i in 1:rows[1]
            for j in 1:column[1]
                neighbours = count_alive(new_board, i, j)
                if new_board[i][j] == 1
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
            new_board[x][y]  = 1 - new_board[x][y]
        end
    
    end
    return new_board
end

function print_as_grid(input_array,output_array,expected,count,generations)
    if output_array==expected
        println("Test Case ", count," has passed")
    else
        println("Test Case ", count," has failed")
        faulty+=1
    end

    println("Input Array ===> Output Array ===> Expected Output")
    println("This test case has ",generations," generations")
    println(input_array[1]," ===>",output_array[1]," ===>",expected[1])
    println(input_array[2]," ===>",output_array[2]," ===>",expected[2])
    println(input_array[3]," ===>",output_array[3]," ===>",expected[3])
    println(input_array[4]," ===>",output_array[4]," ===>",expected[4])

    println("********************************************************")
end

function run_test_cases()
    input1 = [[0,1,0],[0,0,1],[1,1,1],[0,0,0]]
    expected1 = [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
    input2 = [[1,1,0,1],[0,0,0,1],[1,0,1,1],[0,1,0,0]]
    expected2 = [[0,0,1,0],[1,0,0,1],[0,1,1,1],[0,1,1,0]]
    input3 = [[1,0,0,1],[1,1,0,1],[1,0,0,1],[0,1,1,0]]
    expected3 = [[1,1,1,0],[1,1,0,1],[1,0,0,1],[0,1,1,0]]
    input4 = [[1,0,0],[0,0,1],[1,0,0],[0,0,1]]
    expected4 = [[0,0,0],[0,1,0],[0,1,0],[0,0,0]]
    input5 = [[1,1,0,0,0],[0,0,0,0,1],[0,0,0,1,1],[0,0,0,0,0]]
    expected5 = [[0,0,0,0,0],[0,0,0,1,1],[0,0,0,1,1],[0,0,0,0,0]]
    
    output1 = gameOfLife(input1,1)
    output2 = gameOfLife(input2,1)
    output3 = gameOfLife(input3,1)
    output4 = gameOfLife(input4,1)
    output5 = gameOfLife(input5,1)
    
    print_as_grid(input1,output1,expected1,1,1)
    print_as_grid(input2,output2,expected2,2,1)
    print_as_grid(input3,output3,expected3,3,1)
    print_as_grid(input4,output4,expected4,4,1)
    print_as_grid(input5,output5,expected5,5,1)

end

run_test_cases()

