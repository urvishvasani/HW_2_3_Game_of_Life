# Conway's Game of Life in Kotlin, Julia and Haskell

[![Run on Repl.it](https://repl.it/badge/github/urvishvasani/HW_2_3_Game_of_Life)](https://repl.it/github/urvishvasani/HW_2_3_Game_of_Life)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3996733.svg)](https://doi.org/10.5281/zenodo.3996733)
[![Build Status](https://travis-ci.org/urvishvasani/HW_2_3_Game_of_Life.svg?branch=master)](https://travis-ci.org/urvishvasani/HW_2_3_Game_of_Life)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


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
4. **6 test cases** have been added to the implementation and debugger would need to resolve 3 bugs inorder to solve them.

### Run [Haskell implementation](code/haskell/GOL.hs)
1. select language **Haskell** from the drop down.
2. In **Configure the run button** text box, enter ```ghc -dynamic code/haskell/GOL.hs && ./code/haskell/GOL```.
3. Hit **Run** to run the code.

## Script for experimentation
As part of this homework, we have introduced 2 bugs in each implementation of Game of Life: Kotlin, Julia and Haskell. 

1. Compilation bug: You will only be able to run the code successfully once you fix the bug. This bug will be more language specific with an intention to analyze how helpful the compiler error messages are while debugging this type of bugs.
2. Logical bug: This bug would be more specific to the implementation and once you fix this bug, all tests should pass. Main purpose behind introducing this type of bug is to understand how readable and friendly the language is.

Your goal is to debug each implementation and you will get a total of 30 minutes to complete the experiment. Detailed steps to run these three implementations can be found [here](#how-to-run).
Once you are done with your experiment, please fill out this [survey](https://docs.google.com/forms/d/e/1FAIpQLSeqIzBdJArD6M2HLxb0OIcmEGPh17jvUO845rWSREaaegU3qQ/viewform?usp=sf_link) which should take around 2-3 minutes. 

## Observations

While participants are debugging the code in each language, we will record the following data:

- For each language, were they able to find compilation bug? If yes, were they able to fix the bug completely? How much time did they take?
- For each language, were they able to find logical bug? If yes, were they able to fix the bug completely? How much time did they take?
- How useful the traditional backtracking approach was to get to the root cause of the error for each language?


## Team Members
```
Kenil Shah
Urvish Vasani
Faraaz Kakiwala
Raj Shrivastava
Harshit Patel
```
