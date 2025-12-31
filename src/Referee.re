open Game;
open Player;

module Referee =
       (
         MyGame: Game,
         Player1: Player with module PlayerGame = MyGame,
         Player2: Player with module PlayerGame = MyGame,
       ) => {
  /* Change these module names to what you've named them */
  module CurrentGame = MyGame;

  let playGame = (): unit => {
    let rec gameLoop: CurrentGame.state => unit = (
      fun
      | s => {
          print_endline(CurrentGame.stringOfState(s));
          switch (CurrentGame.gameStatus(s)) {
          | CurrentGame.Win(player) =>
            print_endline(CurrentGame.stringOfPlayer(player) ++ " wins!")
          | CurrentGame.Ongoing(player) =>
            print_endline(CurrentGame.stringOfPlayer(player) ++ "'s turn.");
            let theMove =
              switch (player) {
              | CurrentGame.P1 => Player1.nextMove(s)
              | CurrentGame.P2 => Player2.nextMove(s)
              };
            print_endline(
              CurrentGame.stringOfPlayer(player)
              ++ " makes the move "
              ++ CurrentGame.stringOfMove(theMove),
            );
            gameLoop(CurrentGame.nextState(s, theMove));
          };
        }:
        CurrentGame.state => unit
    );
    try(gameLoop(CurrentGame.initialState("TT...FF"))) {
    | Failure(message) => print_endline(message)
    };
  };
};

module R1 =
  Referee(
    TNF.TNF,
    (HumanPlayer.HumanPlayer(TNF.TNF)),
    (HumanPlayer.HumanPlayer(TNF.TNF)),
  );

R1.playGame();
