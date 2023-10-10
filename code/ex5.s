SETI R0, #0             @ Initialize R0 to 0
SETI R1, #1             @ Initialize R1 to 1

@width r2               @ store the width of the map in R2
@height r3              @ store the height of the map in R3
INVOKE 1, 2, 3          @ Get the size of the grid (width and height) and store them in R2 and R3

@ i
SETI R4, #0             @ Initialize R4 (i) to 0
@ j 
SETI R5, #0             @ Initialize R5 (j) to 0

L1:                     @ Start of loop for i
    GOTO_GE L2, R4, R2  @ If i >= width, exit the loop
    INVOKE 3, 4, 5      @ Move to the current cell (i, j)

    @ Get the value of the cell and store it in R6
    INVOKE 4, 6, 0

    @ If the cell is 1, set it to 0
    GOTO_EQ LBORN, R6, R1  @ If the current cell is equal to 1, go to LBORN
    GOTO LDIE              @ Otherwise, go to LDIE

LBORN:                  @ If the cell is equal to 1
    INVOKE 4, 0, 0       @ Set the current cell value to 0

    @ Check if any of the neighbor cells is 1
    INVOKE 5, 7, 3       @ Get the value of the left cell and store it in R7
    GOTO_EQ LNEIGHBOUR, R7, R1  @ If the left cell is equal to 1, go to LNEIGHBOUR
    INVOKE 5, 8, 3       @ Get the value of the top cell and store it in R8
    GOTO_EQ LNEIGHBOUR, R8, R1  @ If the top cell is equal to 1, go to LNEIGHBOUR
    INVOKE 5, 9, 3       @ Get the value of the right cell and store it in R9
    GOTO_EQ LNEIGHBOUR, R9, R1  @ If the right cell is equal to 1, go to LNEIGHBOUR
    INVOKE 5, 10, 3       @ Get the value of the bottom cell and store it in R10
    GOTO_EQ LNEIGHBOUR, R10, R1  @ If the bottom cell is equal to 1, go to LNEIGHBOUR

    INVOKE 5, 11, 3       @ Get the value of the top left cell and store it in R11
    GOTO_EQ LNEIGHBOUR, R11, R1  @ If the top left cell is equal to 1, go to LNEIGHBOUR
    INVOKE 5, 12, 3       @ Get the value of the top right cell and store it in R12
    GOTO_EQ LNEIGHBOUR, R12, R1  @ If the top right cell is equal to 1, go to LNEIGHBOUR
    INVOKE 5, 13, 3       @ Get the value of the bottom left cell and store it in R13
    GOTO_EQ LNEIGHBOUR, R13, R1  @ If the bottom left cell is equal to 1, go to LNEIGHBOUR
    INVOKE 5, 14, 3       @ Get the value of the bottom right cell and store it in R14
    GOTO_EQ LNEIGHBOUR, R14, R1  @ If the bottom right cell is equal to 1, go to LNEIGHBOUR

    GOTO LDIE              @ Otherwise, go to LDIE

LNEIGHBOUR:           @ If one of the neighbor cells is 1
    INVOKE 4, 1, 0       @ Set the current cell value to 1

LDIE:                 @ If the cell is 0 and none of the neighbor cells is 1
    ADD R4, R4, R1       @ Increment i
    GOTO L1              @ Go back to the beginning of the loop

L2:                     @ End of loop for i
    SET R4, R0           @ Reset i to 0
    ADD R5, R5, R1       @ Increment j
    GOTO_GE L3, R5, R3   @ If j >= height, exit the loop and the program
    GOTO L1              @ Otherwise, go back to the beginning of the i loop

L3:                     @ End of loop for j

STOP