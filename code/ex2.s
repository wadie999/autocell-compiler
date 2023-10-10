SETI R0, #0             @ Initialize R0 to 0
SETI R1, #1             @ Initialize R1 to 1

@width r2               @ Store the width of the map in R2
@height r3              @ Store the height of the map in R3

INVOKE 1, 2, 3          @ Get the size of the grid (width and height) and store them in R2 and R3
SUB R2, R2, R1          @ Subtract 1 from the width (R2)
SUB R3, R3, R1          @ Subtract 1 from the height (R3)

INVOKE 3, 0, 0          @ Move to the top-left cell (0, 0)
INVOKE 4, 1, 0          @ Set the top-left cell value to 1

INVOKE 3, 0, 3          @ Move to the top-right cell (0, 10)
INVOKE 4, 1, 0          @ Set the top-right cell value to 1

INVOKE 3, 2, 0          @ Move to the bottom-left cell (10, 0)
INVOKE 4, 1, 0          @ Set the bottom-left cell value to 1

INVOKE 3, 2, 3          @ Move to the bottom-right cell (10, 10)
INVOKE 4, 1, 0          @ Set the bottom-right cell value to 1

STOP                    @ End the program
