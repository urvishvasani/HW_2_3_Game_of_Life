
class GameOfLifeUniverse( val rows: Int, val cols: Int, activeCells: Array<Array<Int>>) {

    var universe = Array(rows) { IntArray(cols) }

    init {
        for (cell in activeCells) {
            universe[cell.get(0)][cell.get(1)] = 1
        }
    }

    fun printUniverse() {
        for (row in universe) {
            for (colum in row) {
                print("$colum ")
            }
            println()
        }
    }

    fun assertEqualsUniverse(compareTo: GameOfLifeUniverse): Boolean{
        println("Expected state of universe:")
        compareTo.printUniverse()
        for (i in 0..rows-1) {
            for (j in 0..cols - 1) {
                if (universe[i][j] != compareTo.universe[i][j]){
                    println("Assertion failed.")
                    return false
                }
            }
        }
        println("Assertion passed.")
        return true
    }

    fun neighbourActiveCount(row: Int, col: Int): Int{
        val neighbours = arrayOf(
            intArrayOf(0,1),
            intArrayOf(1,0),
            intArrayOf(1,1),
            intArrayOf(0,-1),
            intArrayOf(-1,0),
            intArrayOf(-1,-1),
            intArrayOf(1,-1),
            intArrayOf(-1,1))
        var activeNeighboutCount = 0
        for (neighbour in neighbours){
            val neighbour_x = row + neighbour.get(0)
            val neighbour_y = col + neighbour.get(1)
            if (neighbour_x >= 0 && neighbour_y >= 0 && neighbour_x < rows && neighbour_y < cols ){
                if (universe[neighbour_x][neighbour_y] == 1){
                    activeNeighboutCount = activeNeighboutCount + 1
                }
            }
        }
        return activeNeighboutCount
    }

    fun generateNext(){
        var activeNeighbors: Int
        var changedCells: Array<Array<Int>> = arrayOf<Array<Int>>()
        for (i in 0..rows-1){
            for (j in 0..cols-1){
                activeNeighbors = neighbourActiveCount(i,j)
                if (universe[i][j]==1){
                    if (activeNeighbors < 2 || activeNeighbors > 3){
                        changedCells = changedCells + arrayOf(i,j)
                    }
                } else{
                    if (activeNeighbors == 3){
                        changedCells = changedCells + arrayOf(i,j)
                    }
                }
            }
        }
        for (changedCell in changedCells) {
            universe[changedCell.get(0)][changedCell.get(1)] = 1 - universe[changedCell.get(0)][changedCell.get(1)]
        }
    }

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
}

fun main(args: Array<String>) {

    println("Test case 0:")
    println("-------------")
    val universe0_input = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 1),
            arrayOf(1, 1),
            arrayOf(2, 1)
    )).gameOfLife(4)
    println("output:")
    universe0_input.printUniverse()
    val universe0_expected = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 1),
            arrayOf(1, 1),
            arrayOf(2, 1)
    ))
    universe0_input.assertEqualsUniverse(universe0_expected)
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
    println("output:")
    universe1_input.printUniverse()
    val universe1_expected = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(1, 0),
            arrayOf(1, 2),
            arrayOf(2, 1),
            arrayOf(2, 2),
            arrayOf(3, 1)
    ))
    universe1_input.assertEqualsUniverse(universe1_expected)
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
    println("output:")
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
    universe2_input.assertEqualsUniverse(universe2_expected)
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
    println("output:")
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
    universe3_input.assertEqualsUniverse(universe3_expected)
    println()

    println("Test case 4:")
    println("-------------")
    val universe4_input = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(0, 0),
            arrayOf(1, 2),
            arrayOf(2, 0),
            arrayOf(3, 2)
    )).gameOfLife(1)
    println("output:")
    universe4_input.printUniverse()
    val universe4_expected = GameOfLifeUniverse(4,3, arrayOf(
            arrayOf(1, 1),
            arrayOf(2, 1)
    ))
    universe4_input.assertEqualsUniverse(universe4_expected)
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
    println("output:")
    universe5_input.printUniverse()
    val universe5_expected = GameOfLifeUniverse(4,5, arrayOf(
            arrayOf(1, 3),
            arrayOf(1, 4),
            arrayOf(2, 3),
            arrayOf(2, 4)
    ))
    universe5_input.assertEqualsUniverse(universe5_expected)
    println()

}
