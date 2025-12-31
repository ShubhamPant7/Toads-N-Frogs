B02030672 AND B02076893


1. Instructions for User

To play against an AI player, we edit the R1 module in the Referee.re file
to say: 

module R1 =
  Referee(
    TNF.TNF,
    (HumanPlayer.HumanPlayer(TNF.TNF)),
    (AIPlayer.AIPlayer(TNF.TNF)),
  );

where P1 is a HumanPlayer and P2 is an AIPlayer. 
(Similarly for 2 AI players to play each other, both HumanPlayers will be 
changed to AIPlayer or vice versa for HumanPlayer)

When the game starts, P1 is shown the board and prompted for a move. P1 will 
enter an integer index of the piece they want to move, with 0 representing their
"do Nothing" move.

2. Overview of how the Program works

The game is structured around three major components, the Game module (TNF.re), 
the player modules (Human or AI) and the Referee module. 

The Game module (TNF) : 

Defines:

What the board looks like
What a legal move is for each player 
How a move changes the board 
When the game ends 
How to evaluate a board position (used by the AI)

It contains no looping behaviour, and is purely functional 

The Human Player prompts the user for an input and converts the entered string 
into a move 

The AI Player uses a depth limited minimax algorithm with alpha beta pruning 
to choose a move that maximizes or minimizes the evaluation score, 
depending on whether its P1 or P2

These do not control the flow of the game, they only produce moves. 

The Referee Module contains the actual Game Loop

It: 

1. Prints the current state 
2. Checks if the game is over
    If someone has won, it announces the winner and stops 
3. Determines whose turn it is
4. Asks that player for a move 
    HumanPlayer will type a move 
    AIPlayer will calculate the best move 
5. Print the chosen move 
6. Generate the next state using the Game module 
7. Recursively call the loop with the new state 
8. Repeat until the game ends 


3. There are no observable bugs or problems with our program 

4. Discussed with Advay Bajaj, Atharv Parashar, and Rishi Venkatesh

5. The project has no extra features 