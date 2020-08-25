/**
 * GameOfLifeUniverse encapsulates universe and few methods to implement Conway's Game of Life.
 * [rows] and [cols] defines the size of the universe.
 * [activeCells] are coordinates of the cells which are alive initially.
 * [universe] is a 2 dimensional grid where each cell can have values 0 or 1. 1 is the cell is alive and 0 otherwise.
 */
class GameOfLifeUniverse( val rows: Int, val cols: Int, val activeCells: Array<Array<Int>>) {

    var universe = Array(rows) { IntArray(cols) }

    init {
        for (cell in activeCells) {
            universe[cell.get(0)][cell.get(1)] = 1
        }
    }

    /**
     * Entry point for the Game of Life. Evolves universe with [numOfGenerations] generations and returns same object.
     */
    fun gameOfLife(numOfGenerations: Int): GameOfLifeUniverse {
        println("Initial state of universe:")
        printUniverse()
        for (i in 0..numOfGenerations-1){
            generateNext()
            //println("after iteration ${i+1}, this is the state of universe:")
            //printUniverse()
        }
        return this
    }

    /**
     * Produces the next generation from the current state of universe and
     * updates the same universe object using following rules:
     *  1. Any live cell with fewer than two live neighbors dies, as if caused by under-population.
     *  2. Any live cell with two or three live neighbors lives on to the next generation.
     *  3. Any live cell with more than three live neighbors dies, as if by over-population.
     *  4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
     *
     *  In order to make the updates in place, values which needs to be updated to 0 are updated temporarily with 2
     *  and values which needs to be updated to 1 are updated temporarily with 3.
     *  At the end of this function, these values are set back to the values that are intended.
     */
    fun generateNext(){
        var activeNeighbors: Int
        for (i in 0..rows-1){
            for (j in 0..cols-1){
                activeNeighbors = countActiveNeighbour(i,j)
                if (universe[i][j]==1){
                    if (activeNeighbors < 2 || activeNeighbors > 3){
                        universe[i][j] = 3
                    }
                } else if (universe[i][j]==0) {
                    if (activeNeighbors == 3){
                        universe[i][j] = 2
                    }
                }
            }
        }
        for (i in 0..rows-1){
            for (j in 0..cols-1){
                if (universe[i][j]==2){
                    universe[i][j] = 0
                } else if (universe[i][j]==3) {
                    universe[i][j] = 1
                }
            }
        }
    }

    /**
     * Returns the number of active neighbours for the cell identified by [row] and [cols].
     */
    fun countActiveNeighbour(row: Int, col: Int): Int{
        val neighbours = arrayOf(
                intArrayOf(0,1),
                intArrayOf(1,0),
                intArrayOf(1,1),
                intArrayOf(0,-1),
                intArrayOf(-1,0),
                intArrayOf(-1,-1),
                intArrayOf(1,-1),
                intArrayOf(-1,1))
        var activeNeighbourCount = 0
        for (neighbour in neighbours){
            val neighbour_x = row + neighbour.get(0)
            val neighbour_y = col + neighbour.get(1)
            if (neighbour_x >= 0 && neighbour_y >= 0 && neighbour_x < rows && neighbour_y < cols ){
                if (universe[neighbour_x][neighbour_y] == 1){
                    activeNeighbourCount += 1
                }
            }
        }
        return activeNeighbourCount
    }

    /**
     * displays the universe associated with this GameOfLifeUniverse object.
     */
    fun printUniverse() {
        for (row in universe) {
            for (colum in row) {
                print("$colum ")
            }
            println()
        }
    }

    /**
     * Compares the universe associated with this object to the universe of the
     * [compareTo] object for the equality.
     *
     * returns true if both universe are equal and false otherwise.
     */
    fun assertEqualsUniverse(compareTo: GameOfLifeUniverse): Boolean{
        println("Expected state of universe:")
        compareTo.printUniverse()
        for (i in 0..rows-1) {
            for (j in 0..cols - 1) {
                if (universe[i][j] != compareTo[i][j]){
                    println("Assertion failed.")
                    return false
                }
            }
        }
        println("Assertion passed.")
        return true
    }
}

fun main(args: Array<String>) {

    var test_case_status = arrayOf<Boolean>()

    println()
    println("Tests started running......")
    println()
    println("Test case 0:")
    println("-------------")
    val universe0_input = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 1),
            arrayOf(1, 1),
            arrayOf(2, 1)
    )).gameOfLife(2)
    println("output after 2 generations:")
    universe0_input.printUniverse()
    val universe0_expected = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 1),
            arrayOf(1, 1),
            arrayOf(2, 1)
    ))
    test_case_status += universe0_input.assertEqualsUniverse(universe0_expected)
    println()


    println("Test case 1:")
    println("-------------")
    val universe1_input = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 1),
            arrayOf(1, 2),
            arrayOf(2, 0),
            arrayOf(2, 1),
            arrayOf(2, 2)
    )).gameOfLife(1)
    println("output after 1 generation:")
    universe1_input.printUniverse()
    val universe1_expected = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(1, 0),
            arrayOf(1, 2),
            arrayOf(2, 1),
            arrayOf(2, 2),
            arrayOf(3, 1)
    ))
    test_case_status += universe1_input.assertEqualsUniverse(universe1_expected)
    println()

    println("Test case 2:")
    println("-------------")
    val universe2_input = GameOfLifeUniverse(4,4, arrayOf(
            arrayOf(0, 0),
            arrayOf(0, 1),
            arrayOf(0, 3),
            arrayOf(1, 3),
            arrayOf(2, 0),
            arrayOf(2, 2),
            arrayOf(2, 3),
            arrayOf(3, 1)
    )).gameOfLife(1)
    println("output after 1 generation:")
    universe2_input.printUniverse()
    val universe2_expected = GameOfLifeUniverse(4,4, arrayOf(
            arrayOf(0, 2),
            arrayOf(1, 0),
            arrayOf(1, 3),
            arrayOf(2, 1),
            arrayOf(2, 2),
            arrayOf(2, 3),
            arrayOf(3, 1),
            arrayOf(3, 2)
    ))
    test_case_status += universe2_input.assertEqualsUniverse(universe2_expected)
    println()

    println("Test case 3:")
    println("-------------")
    val universe3_input = GameOfLifeUniverse(4,4, arrayOf(
            arrayOf(0, 0),
            arrayOf(0, 3),
            arrayOf(1, 0),
            arrayOf(1, 1),
            arrayOf(1, 3),
            arrayOf(2, 0),
            arrayOf(2, 3),
            arrayOf(3, 1),
            arrayOf(3, 2)
    )).gameOfLife(1)
    println("output after 1 generation:")
    universe3_input.printUniverse()
    val universe3_expected = GameOfLifeUniverse(4,4, arrayOf(
            arrayOf(0, 0),
            arrayOf(0, 1),
            arrayOf(0, 2),
            arrayOf(1, 0),
            arrayOf(1, 1),
            arrayOf(1, 3),
            arrayOf(2, 0),
            arrayOf(2, 3),
            arrayOf(3, 1),
            arrayOf(3, 2)
    ))
    test_case_status += universe3_input.assertEqualsUniverse(universe3_expected)
    println()

    println("Test case 4:")
    println("-------------")
    val universe4_input = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 0),
            arrayOf(1, 2),
            arrayOf(2, 0),
            arrayOf(3, 2)
    )).gameOfLife(1)
    println("output after 1 generation:")
    universe4_input.printUniverse()
    val universe4_expected = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(1, 1),
            arrayOf(2, 1)
    ))
    test_case_status += universe4_input.assertEqualsUniverse(universe4_expected)
    println()

    println("Test case 5:")
    println("-------------")
    val universe5_input = GameOfLifeUniverse(4,5, arrayOf(
            arrayOf(0, 0),
            arrayOf(0, 1),
            arrayOf(1, 4),
            arrayOf(2, 3),
            arrayOf(2, 4)
    )).gameOfLife(1)
    println("output after 1 generation:")
    universe5_input.printUniverse()
    val universe5_expected = GameOfLifeUniverse(4,5, arrayOf(
            arrayOf(1, 3),
            arrayOf(1, 4),
            arrayOf(2, 3),
            arrayOf(2, 4)
    ))
    test_case_status += universe5_input.assertEqualsUniverse(universe5_expected)
    println()

    println("Summary of test cases:")
    test_case_status.forEachIndexed { index, b ->
        println("Test Case $index : ${if (b) "Passed" else "Failed"} ")
    }
}
