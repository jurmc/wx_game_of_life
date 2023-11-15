In Powershell: cd C:\Users\amj018\Documents\isi\wx_game_of_life\src
               werl -name win@10.0.2.2 -setcookie <cookie>
In Arch:       erl -name arch@192.168.56.1 -setcookie abc -remsh 'win@10.0.2.2' 
               wx_gof:start_link(5).

Game of Life

Your task is to write a program to calculate the next generation of Conway's game of life, given any starting position. You start with a two dimensional grid of cells, where each cell is either alive or dead. The grid is finite, and no life can exist off the edges. When calculating the next generation of the grid, follow these four rules:

1. Any live cell with fewer than two live neighbors dies, as if caused by underpopulation.

2. Any live cell with more than three live neighbors dies, as if by overcrowding.

3. Any live cell with two or three live neighbors lives on to the next generation.

4. Any dead cell with exactly three live neighbors becomes a live cell.

Examples: * indicates live cell, . indicates dead cell

Example input: (4 x 8 grid)

4 8

........
....*...
...**...
........

Example output:

4 8

........
...**...
...**...
........
