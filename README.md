# Conway's Game of Life in Kotlin, Julia and Haskell

[![Run on Repl.it](https://repl.it/badge/github/urvishvasani/HW_2_3_Game_of_Life)](https://repl.it/github/urvishvasani/HW_2_3_Game_of_Life)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3996733.svg)](https://doi.org/10.5281/zenodo.3996733)
[![Build Status](https://travis-ci.org/urvishvasani/HW_2_3_Game_of_Life.svg?branch=master)](https://travis-ci.org/urvishvasani/HW_2_3_Game_of_Life)

This repo has been created for the CSC 510 - Software Engineering Fall 2020 course's Homework 2 and 3 which implements Conway's Game of Life in three programming languages: Kotlin, Julia and Haskell.

## Game of Life
According to the [Wikipedia's article](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life): "The Game of Life, also known simply as Life, is a cellular automaton devised by the British mathematician John Horton Conway in 1970."

Given a board with m by n cells, each cell has an initial state live (1) or dead (0). Each cell interacts with its eight neighbors (horizontal, vertical, diagonal) using the following four rules (taken from the above Wikipedia article):

- Any live cell with fewer than two live neighbors dies, as if caused by under-population.
- Any live cell with two or three live neighbors lives on to the next generation.
- Any live cell with more than three live neighbors dies, as if by over-population..
- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.

<p align="center">
  <img src="https://github.com/urvishvasani/HW_2_3_Game_of_Life/blob/master/data/GOL.gif">
</p>

## Team Members
```
Kenil Shah
Urvish Vasani
Faraaz Kakiwala
Raj Shrivastava
Harshit Patel
```

## How to run?

### Basic setup
Click  [![Run on Repl.it](https://repl.it/badge/github/urvishvasani/HW_2_3_Game_of_Life)](https://repl.it/github/urvishvasani/HW_2_3_Game_of_Life)  to clone and run this project on an online IDE [repl it](https://repl.it/repls/AjarGrubbyProgramminglanguage#README.md).

### Run [kotlin implementation](code/kotlin/GOL.kt)
1. select language **Kotlin** from the drop down.
2. In **Configure the run button** text box, enter ```kotlinc -d main.jar code/kotlin/GOL.kt && kotlin -classpath main.jar GOLKt```.
3. Hit **Run** to run the code.

### Run [Julia implementation](code/julia/GOL.jl)
1. select language **Julia** from the drop down.
2. In **Configure the run button** text box, enter ```julia code/julia/GOL.jl```.
3. Hit **Run** to run the code.

### Run [Haskell implementation]()
1. select language **Haskell** from the drop down.


