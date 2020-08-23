
class GameOfLife( val rows: Int, val cols: Int) {

    var universe = Array(rows) { IntArray(cols) }

    fun printUniverse() {
        for (row in universe) {
            for (colum in row) {
                print("$colum ")
            }
            println()
        }
    }
    fun initalActives(activeCells: Array<Array<Int>>) {
        for (cell in activeCells) {
            universe[cell.get(0)][cell.get(1)] = 1
        }
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
        var activeNeighboutCount: Int = 0
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

    fun gameOfLife(){
        var activeNeighbors: Int = 0
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
        printUniverse()
    }
}

fun main(args: Array<String>) {
    var gol = GameOfLife(4,3)
    val activeCells = arrayOf(
        arrayOf(0, 1),
        arrayOf(1, 2),
        arrayOf(2, 0),
        arrayOf(2, 1),
        arrayOf(2, 2)
    )
    gol.initalActives(activeCells)
    gol.gameOfLife()
}
