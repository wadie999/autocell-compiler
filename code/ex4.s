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

    @ Get the value of the left cell and store it in R6
    INVOKE 5, 6, 3

    GOTO_EQ LBORN, R6, R1  @ If the left cell is equal to 1, go to LBORN
    GOTO LDIE             @ Otherwise, go to LDIE

LBORN:                  @ If the left cell is equal to 1
    INVOKE 4, 1, 0       @ Set the current cell value to 1

LDIE:                 @ If the left cell is not equal to 1
    ADD R4, R4, R1       @ Increment i
    GOTO L1              @ Go back to the beginning of the loop

L2:                     @ End of loop for i
    SET R4, R0           @ Reset i to 0
    ADD R5, R5, R1       @ Increment j
    GOTO_GE L3, R5, R3   @ If j >= height, exit the loop and the program
    GOTO L1              @ Otherwise, go back to the beginning of the i loop

L3:                     @ End of loop for j
    STOP                 @ End the program
