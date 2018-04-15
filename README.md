# Connect4Game
The objective of this project is apply search algorithms and their applications in path finding problems.The algorithms are implemented using Haskell.

I have implemented minimax search and minimax search with alpha-beta pruning (see Chapter 5 of R&N) for the two-player game Connect Four. The board size is 4 columns and 4 rows..
Rules introduction:
1. Two players take turns adding one piece at a time.
2. For each column, only the lowest unoccupied cell is the available space for a player to add a piece. An example is given by Figure 3.
3. The winner is the player who is the first to form a horizontal, vertical, or diagonal line of four of one’s own pieces. If the game board fills before either player achieves four in a row, then the game is a draw.


• ghci GHCi is the interactive environment, in which Haskell expressions can be interactively evaluated and programs can be interpreted. You can start GHCi with the command ghci in a terminal. All following commands work in GHCi environment.
• :help Get help information.
• :cd <dir> You can save *.hs files anywhere you like, but if you save it somewhere other than the current
directory, then you will need to change to the right directory, the directory (or folder) in which you saved *.hs.
• :show paths Show the current directory.
• :load Main To load a Haskell source file into GHCi. For short, you can also use :l Main, where Main is the topmost module in our assignment.
• main Run function main.
  
  
