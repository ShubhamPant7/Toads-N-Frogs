open! CS17SetupGame;
open Game;

module AIPlayer = (MyGame: Game) => {
  module PlayerGame = MyGame;
  /* TODO */
  /* maxDepth represents the maximum depth to which the minimax algorithm runs*/
  let maxDepth = 5;
  /* maxVal represents the maximum value for estimateValue */
  let maxVal = 1e9;
  /* minVal represents the minimum value for estimateValue */
  let minVal = (-1e9);

  /* alphaBetaMinimax: (PlayerGame.state, int, float, float) => float
      Input: the state, s an int (depth) the depth, alpha, the best value that
             the maximizing player (P1) has found so far and beta, the best value
             that the minimizing player (P2) has found so far.
      Output: a float, representing the estimated value of the best achievable
              outcome for the current player, assuming optimal play from both
              sides.

     Recursion Diagram #1:
        OI: ({board: [Empty, Empty], turn: P1, usedNothing: {p1: false, p2: false},}), 0, infinity, -infinity
        RI: NA (depth is 0)
        RO: NA
        Ideation: NA
        OO: estimateValue({board: [Empty, Empty], turn: P1, usedNothing: {p1: false, p2: false},})

        Recursion Diagram #2:
        OI: (
        {board: [Frog, Frog, Frog, Empty, Toad, Empty, Toad, Toad],
        turn: P1,
        usedNothing: {p1: false, p2: true},}, 1, infinity, -infinity
        RI: NA (gameStatus is a Win for P1 because Frogs are trapped)
        RO: NA
        Ideation: NA
        OO: estimateValue({board: [Frog, Frog, Frog, Empty, Toad, Empty, Toad, Toad],
          turn: P1, usedNothing: {p1: false, p2: true},})

     */

  let rec alphaBetaMinimax: (PlayerGame.state, int, float, float) => float =
    (s, depth, alpha, beta) => {
      let possibleLegalMoves = PlayerGame.legalMoves(s);
      switch (PlayerGame.gameStatus(s), possibleLegalMoves) {
      | (Win(_), _)
      | (_, []) => PlayerGame.estimateValue(s)
      | (Ongoing(player), alom) =>
        if (player == P1) {
          let rec returnMax: (list(PlayerGame.move), float, float) => float = (
            (alom, alphaVal, bestVal) =>
              switch (alom) {
              | [] => bestVal
              | [hd, ...tl] =>
                let currVal =
                  alphaBetaMinimax(
                    PlayerGame.nextState(s, hd),
                    depth - 1,
                    alphaVal,
                    beta,
                  );
                let bestOverall = max(currVal, bestVal);
                let bestAlpha = max(currVal, alphaVal);

                if (beta <= bestAlpha) {
                  bestOverall;
                } else {
                  returnMax(tl, bestAlpha, bestOverall);
                };
              }
          );
          returnMax(alom, alpha, minVal);
        } else {
          let rec returnMin: (list(PlayerGame.move), float, float) => float = (
            (alom, betaVal, bestVal) =>
              switch (alom) {
              | [] => bestVal
              | [hd, ...tl] =>
                let currVal =
                  alphaBetaMinimax(
                    PlayerGame.nextState(s, hd),
                    depth - 1,
                    alpha,
                    betaVal,
                  );
                let bestOverall = min(currVal, bestVal);
                let bestBeta = min(currVal, betaVal);

                if (bestBeta <= alpha) {
                  bestOverall;
                } else {
                  returnMin(tl, bestBeta, bestOverall);
                };
              }
          );
          returnMin(alom, beta, maxVal);
        }
      };
    };

  /* nextMove: PlayerGame.state => PlayerGame.move
           Input: the state of the game
           Output: the best move for the AI Player, based on the minimax algorithm
     */
  let nextMove: PlayerGame.state => PlayerGame.move =
    s => {
      let rec minimax: (PlayerGame.state, int) => float =
        (s, depth) =>
          switch (PlayerGame.gameStatus(s)) {
          | Win(_) => PlayerGame.estimateValue(s)
          | Ongoing(player) =>
            if (depth == 0) {
              PlayerGame.estimateValue(s);
            } else {
              let allowedMoves = PlayerGame.legalMoves(s);
              let stateScores =
                List.map(
                  x => minimax(PlayerGame.nextState(s, x), depth - 1),
                  allowedMoves,
                );
              if (player == P1) {
                List.fold_left(max, -. infinity, stateScores);
              } else {
                List.fold_left(min, infinity, stateScores);
              };
            }
          };

      switch (PlayerGame.legalMoves(s)) {
      | [] => failwith("THERE ARE NO LEGAL MOVES!")
      | [_, ..._] =>
        let scoresOfPossibleLegalMoves =
          List.map(
            m => (m, minimax(PlayerGame.nextState(s, m), maxDepth)),
            PlayerGame.legalMoves(s),
          );
        let chooseBestMove: (('b, 'b) => bool, list(('a, 'b))) => ('a, 'b) = (
          (valueComparison, movesAndScores) =>
            List.fold_left(
              (currBest, nextPossible) =>
                if (valueComparison(snd(nextPossible), snd(currBest))) {
                  nextPossible;
                } else {
                  currBest;
                },
              List.hd(movesAndScores),
              List.tl(movesAndScores),
            )
        );

        switch (PlayerGame.gameStatus(s)) {
        | Ongoing(P1) =>
          let (bestPossibleMove, _) =
            chooseBestMove((a, b) => a > b, scoresOfPossibleLegalMoves);
          bestPossibleMove;
        | Ongoing(P2) =>
          let (bestPossibleMove, _) =
            chooseBestMove((a, b) => a < b, scoresOfPossibleLegalMoves);
          bestPossibleMove;
        | Win(_) => failwith("GAME IS OVER!")
        };
      };
    };

  /* put your team name here! */
  let playerName = "";
};

module TestGame = TNF.TNF;
open Player;

module TestAIPlayer = AIPlayer(TestGame);
module MyAIPlayer: Player = TestAIPlayer;
open TestAIPlayer;

/* insert test cases for any procedures that don't take in
 * or return a state here */
