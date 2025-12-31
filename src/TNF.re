open CS17SetupGame;
open Game;

module TNF: Game = {
  /* TYPES */
  /* player 1 is P1, player 2 is P2 */
  /* whichPlayer represents which player's turn it is.
        Example Data:
           P1
           P2
     */

  type whichPlayer =
    | P1
    | P2;

  /* either a player has won, it's a draw, or it's ongoing */
  /* status represents what the status of the game is
        Example Data:
           Win(P2)
           Ongoing(P1)
           Win(P1)
     */

  type status =
    | Win(whichPlayer)
    | Ongoing(whichPlayer);

  /* gameSpace represents a cell on the board
        Example Data:
           Toad
           Frog
           Empty
     */

  type gameSpace =
    | Toad
    | Frog
    | Empty;

  /* state represents the state of the game, which includes the board, whose
        turn it is, and which player has their do nothing move remaining
        Example Data:
           {
             board: list([Toad, Toad, Empty, Empty, Empty, Frog, Frog]),
             who: P1,
             usedNothingP1: false,
             usedNothingP2: false,
           };

           {
             board: list([Toad, Empty, Toad, Empty, Empty, Frog, Frog]),
             who: P2,
             usedNothingP1: true,
             usedNothingP2: true,
           };
     */
  type state = {
    board: list(gameSpace),
    who: whichPlayer,
    usedNothingP1: bool,
    usedNothingP2: bool,
  };

  /* move represents the piece the player wants to move, as an integer
        Example Data:
           1
           6
           4
     */
  type move = int;

  /* INITIAL GAME STATE */
  /*
   initialState: string => state
      Input: a string, that represents the board of the game
      Output: the state of the board initially (with P1 starting always) and
              the do nothing move usage for both players set to false
    */

  let initialState: string => state =
    s => {
      let convertIntToGameSpace: int => gameSpace =
        fun
        | 0 => Empty
        | 1 => Toad
        | 2 => Frog
        | _ => failwith("INVALID ");

      let parsed = parseInput(s);

      let initialBoard = List.map(convertIntToGameSpace, parsed);

      {
        board: initialBoard,
        who: P1, /* since player 1 always starts */
        usedNothingP1: false,
        usedNothingP2: false,
      };
    };

  /* Check Expects for initialState */

  checkExpect(
    initialState("TT..FF"),
    {
      board: [Toad, Toad, Empty, Empty, Frog, Frog],
      who: P1,
      usedNothingP1: false,
      usedNothingP2: false,
    },
    "Success!",
  );

  checkExpect(
    initialState("TTT....FFF"),
    {
      board: [Toad, Toad, Toad, Empty, Empty, Empty, Empty, Frog, Frog, Frog],
      who: P1,
      usedNothingP1: false,
      usedNothingP2: false,
    },
    "Success!",
  );

  checkError(
    () => initialState("TTT....XXX"),
    "invalid character in initialization string",
  );

  /* TYPE CONVERSIONS */
  /* stringOfPlayer; whichPlayer => string
      Input: the player whose turn it is, as a whichPlayer type
      Output: the player whose turn it is, as a string
     */

  let stringOfPlayer: whichPlayer => string =
    fun
    | P1 => "P1"
    | P2 => "P2";

  /* Check Expects for stringOfPlayer */

  checkExpect(stringOfPlayer(P1), "P1", "Success!");
  checkExpect(stringOfPlayer(P2), "P2", "Success!");

  /* stringOfState: state => string
      Input: the state of the game, st
      Output: a string of the state of the game
     */

  let stringOfState: state => string =
    st => {
      /* Example: convert board to "X . O" style */
      let cells =
        List.map(
          i =>
            switch (i) {
            | Toad => "T"
            | Frog => "F"
            | Empty => "."
            },
          st.board,
        );
      /* join with spaces */
      String.concat(" ", cells) ++ "\nturn: " ++ stringOfPlayer(st.who);
    };

  /* Check Expects for stringOfState */

  checkExpect(
    stringOfState({
      board: [Toad, Toad, Empty, Empty, Frog, Frog],
      who: P1,
      usedNothingP1: false,
      usedNothingP2: false,
    }),
    "T T . . F F\nturn: P1",
    "Success!",
  );

  checkExpect(
    stringOfState({
      board: [
        Toad,
        Toad,
        Toad,
        Toad,
        Empty,
        Empty,
        Empty,
        Empty,
        Empty,
        Frog,
        Frog,
        Frog,
        Frog,
      ],
      who: P1,
      usedNothingP1: false,
      usedNothingP2: false,
    }),
    "T T T T . . . . . F F F F\nturn: P1",
    "Success!",
  );

  /* stringOfMove: move => string
        stringOfMove represents the move played as a string
           Input: m, a move
           Output: a string of the move m
     */

  let stringOfMove: move => string = m => string_of_int(m);

  /* Check Expects for stringOfMove */

  checkExpect(stringOfMove(6), "6", "Success!");
  checkExpect(stringOfMove(8), "8", "Success!");
  checkExpect(stringOfMove(2), "2", "Success!");

  /* GAME LOGIC */

  /* legalMoves: state => list(move)
           Input: the state of the game
           Output: a list of all the possible legal moves
     */

  let legalMoves: state => list(move) =
    s => {
      let accessGameSpace: (list(gameSpace), int, int) => gameSpace =
        (bd, n, len) =>
          if (n < 1 || n > len) {
            failwith("ILLEGAL MOVE: INDEX IS OUT OF BOUNDS!");
          } else {
            List.nth(bd, n - 1);
          };

      let canToadMove: (list(gameSpace), int, int) => bool =
        (bd, i, len) => {
          i < len
          && accessGameSpace(bd, i + 1, len) == Empty
          || i
          + 1 < len
          && accessGameSpace(bd, i + 1, len) == Frog
          && accessGameSpace(bd, i + 2, len) == Empty;
        };

      let canFrogMove: (list(gameSpace), int, int) => bool =
        (bd, i, len) => {
          i > 1
          && accessGameSpace(bd, i - 1, len) == Empty
          || i > 2
          && accessGameSpace(bd, i - 1, len) == Toad
          && accessGameSpace(bd, i - 2, len) == Empty;
        };

      let bd = s.board;
      let n = List.length(bd);

      let moves =
        switch (s.who) {
        | P1 =>
          if (!s.usedNothingP1) {
            [0];
          } else {
            [];
          }
        | P2 =>
          if (!s.usedNothingP2) {
            [0];
          } else {
            [];
          }
        };

      let rec addLegalMoves: (int, list(move)) => list(move) =
        (index, currentMoves) =>
          if (index > n) {
            currentMoves;
          } else {
            let piece = accessGameSpace(bd, index, n);
            let canPieceMove =
              switch (s.who) {
              | P1 => piece == Toad && canToadMove(bd, index, n)
              | P2 => piece == Frog && canFrogMove(bd, index, n)
              };
            let moves =
              if (canPieceMove) {
                currentMoves @ [index];
              } else {
                currentMoves;
              };
            addLegalMoves(index + 1, moves);
          };
      addLegalMoves(1, moves);
    };

  /* Check Expects for legalMoves */

  checkExpect(
    legalMoves({
      board: [Toad, Toad, Empty, Empty, Frog, Frog],
      who: P1,
      usedNothingP1: false,
      usedNothingP2: false,
    }),
    [0, 2],
    "Success!",
  );

  checkExpect(
    legalMoves({
      board: [
        Toad,
        Toad,
        Toad,
        Empty,
        Empty,
        Toad,
        Empty,
        Empty,
        Frog,
        Empty,
        Frog,
        Frog,
        Frog,
      ],
      who: P2,
      usedNothingP1: true,
      usedNothingP2: true,
    }),
    [9, 11],
    "Success!",
  );

  checkExpect(
    legalMoves({
      board: [Frog, Frog, Empty, Toad, Empty, Toad],
      who: P2,
      usedNothingP1: false,
      usedNothingP2: true,
    }),
    [],
    "Success!",
  );

  /* moveOfString : (string, state) => move
           Input: the move, str as a string and the state of the game, st
           Output: the string as a move
     */
  let moveOfString: (string, state) => move =
    (str, st) => {
      let currMove =
        try(int_of_string(str)) {
        | _ => failwith("NEED TO INPUT AN INTEGER!")
        };

      let rec confirmIfLegal: list(move) => move =
        fun
        | [] => failwith("ILLEGAL MOVE!")
        | [hd, ...tl] =>
          if (hd == currMove) {
            currMove;
          } else {
            confirmIfLegal(tl);
          };

      confirmIfLegal(legalMoves(st));
    };

  /* Check Expects for moveOfString */

  checkExpect(
    moveOfString(
      "2",
      {
        board: [Toad, Toad, Empty, Empty, Frog, Frog],
        who: P1,
        usedNothingP1: false,
        usedNothingP2: false,
      },
    ),
    2,
    "Success!",
  );

  checkExpect(
    moveOfString(
      "5",
      {
        board: [Toad, Toad, Empty, Empty, Frog, Frog],
        who: P2,
        usedNothingP1: false,
        usedNothingP2: false,
      },
    ),
    5,
    "Success!",
  );

  checkExpect(
    moveOfString(
      "0",
      {
        board: [Toad, Toad, Empty, Empty, Frog, Frog],
        who: P1,
        usedNothingP1: false,
        usedNothingP2: false,
      },
    ),
    0,
    "Success!",
  );

  checkError(
    () =>
      moveOfString(
        "X",
        {
          board: [Toad, Toad, Empty, Empty, Frog, Frog],
          who: P1,
          usedNothingP1: false,
          usedNothingP2: false,
        },
      ),
    "NEED TO INPUT AN INTEGER!",
  );

  checkError(
    () =>
      moveOfString(
        "10",
        {
          board: [Toad, Toad, Empty, Empty, Frog, Frog],
          who: P1,
          usedNothingP1: false,
          usedNothingP2: false,
        },
      ),
    "ILLEGAL MOVE!",
  );

  checkError(
    () =>
      moveOfString(
        "3",
        {
          board: [Toad, Toad, Empty, Empty, Frog, Frog],
          who: P1,
          usedNothingP1: false,
          usedNothingP2: false,
        },
      ),
    "ILLEGAL MOVE!",
  );

  /* gameStatus: state => status
           Input: the state of the game, s
           Output: the status of the game, either Win or Ongoing, with the player
                   who won or whose turn it is (if Ongoing)
     */

  let gameStatus: state => status =
    s =>
      switch (legalMoves(s)) {
      | [] =>
        if (s.who == P1) {
          Win(P2);
        } else {
          Win(P1);
        }
      | _ => Ongoing(s.who)
      };

  /* Check Expects for gameStatus */

  checkExpect(
    gameStatus({
      board: [Toad, Toad, Empty, Empty, Frog, Frog],
      who: P1,
      usedNothingP1: false,
      usedNothingP2: false,
    }),
    Ongoing(P1),
    "Success!",
  );

  checkExpect(
    gameStatus({
      board: [Frog, Frog, Empty, Toad, Empty, Toad],
      who: P2,
      usedNothingP1: false,
      usedNothingP2: true,
    }),
    Win(P1),
    "Success!",
  );

  /* nextState: (state, move) => state
           Input: the state of the game, s and the move, m
           Output: the next state of the game
     */

  let nextState: (state, move) => state =
    (s, m) =>
      switch (m) {
      | 0 =>
        switch (s.who) {
        | P1 => {
            board: s.board,
            who: P2,
            usedNothingP1: true,
            usedNothingP2: s.usedNothingP2,
          }
        | P2 => {
            board: s.board,
            who: P1,
            usedNothingP1: s.usedNothingP1,
            usedNothingP2: true,
          }
        }
      | _ =>
        let i = m - 1;
        let boardLength = List.length(s.board);
        let currGameSpace = List.nth(s.board, i);

        let endGameSpace =
          switch (currGameSpace) {
          | Toad =>
            if (i + 1 < boardLength && List.nth(s.board, i + 1) == Empty) {
              i + 1;
            } else {
              i + 2;
            }
          | Frog =>
            if (i - 1 >= 0 && List.nth(s.board, i - 1) == Empty) {
              i - 1;
            } else {
              i - 2;
            }
          | Empty => failwith("LEGAL MOVES CANNOT RETURN AN EMPTY SQUARE!")
          };

        let rec updateGameSpace: (list('a), int, 'a) => list('a) = (
          (alod, index, v) =>
            switch (alod, index) {
            | ([], _) => []
            | ([_, ...tl], 0) => [v, ...tl]
            | ([hd, ...tl], n) when n > 0 => [
                hd,
                ...updateGameSpace(tl, n - 1, v),
              ]
            | _ => alod
            }
        );

        {
          board:
            updateGameSpace(
              updateGameSpace(s.board, i, Empty),
              endGameSpace,
              currGameSpace,
            ),
          who:
            switch (s.who) {
            | P1 => P2
            | P2 => P1
            },
          usedNothingP1: s.usedNothingP1,
          usedNothingP2: s.usedNothingP2,
        };
      };

  /* Check Expects for nextState */

  checkExpect(
    nextState(
      {
        board: [Frog, Toad, Empty, Frog, Empty, Toad],
        who: P1,
        usedNothingP1: false,
        usedNothingP2: true,
      },
      2,
    ),
    {
      board: [Frog, Empty, Toad, Frog, Empty, Toad],
      who: P2,
      usedNothingP1: false,
      usedNothingP2: true,
    },
    "Success!",
  );

  checkExpect(
    nextState(
      {
        board: [Frog, Toad, Empty, Frog, Empty, Toad],
        who: P1,
        usedNothingP1: false,
        usedNothingP2: true,
      },
      0,
    ),
    {
      board: [Frog, Toad, Empty, Frog, Empty, Toad],
      who: P2,
      usedNothingP1: true,
      usedNothingP2: true,
    },
    "Success!",
  );

  /* estimateValue: state => float
           Input: the state of the game
           Output: the evaluation of the state of the game as a float
     */

  let estimateValue: state => float =
    s =>
      switch (gameStatus(s)) {
      | Win(P1) => 1000.0
      | Win(P2) => (-1000.0)
      | Ongoing(player) =>
        let legalMovesDiff =
          if (player == P1) {
            let p2state = {
              board: s.board,
              who: P2,
              usedNothingP1: s.usedNothingP1,
              usedNothingP2: s.usedNothingP2,
            };
            List.length(legalMoves(s))
            * 10
            - List.length(legalMoves(p2state))
            * 10;
          } else {
            let p1state = {
              board: s.board,
              who: P1,
              usedNothingP1: s.usedNothingP1,
              usedNothingP2: s.usedNothingP2,
            };
            List.length(legalMoves(s))
            * 10
            - List.length(legalMoves(p1state))
            * 10;
          };
        let usedNothingDiff =
          (
            if (s.usedNothingP1) {
              0;
            } else {
              40;
            }
          )
          - (
            if (s.usedNothingP2) {
              0;
            } else {
              40;
            }
          );

        let boardLength = List.length(s.board);
        let centerOfBoard = (boardLength + 1) / 2;

        let rec advancementDiffCalc: (list(gameSpace), 'b, 'a) => 'a = (
          (bd, i, acc) =>
            switch (bd) {
            | [] => acc
            | [Toad, ...tl] => advancementDiffCalc(tl, i + 1, acc - 10 * i)
            | [Frog, ...tl] =>
              advancementDiffCalc(tl, i + 1, acc - 10 * (i - boardLength - 1))
            | [Empty, ...tl] => advancementDiffCalc(tl, i + 1, acc)
            }
        );

        let rec centerScoreCalc = (bd, i, acc) =>
          switch (bd) {
          | [] => acc
          | [Toad, ...tl] =>
            let dist = abs(i - centerOfBoard);
            centerScoreCalc(tl, i + 1, acc - (10 - dist));
          | [Frog, ...tl] =>
            let dist = abs(i - centerOfBoard);
            centerScoreCalc(tl, i + 1, acc + (10 - dist));
          | [Empty, ...tl] => centerScoreCalc(tl, i + 1, acc)
          };

        let rec oppBlockScoreCalc = (bd, i, acc) =>
          switch (bd) {
          | [] => acc
          | [_] => acc
          | [Toad, Frog, ...tl] =>
            oppBlockScoreCalc([Frog, ...tl], acc - 20, i + 1)
          | [Frog, Toad, ...tl] =>
            oppBlockScoreCalc([Toad, ...tl], acc + 20, i + 1)
          | [_, ...tl] => oppBlockScoreCalc(tl, acc, i + 1)
          };

        let rec selfBlockScoreCalc = (bd, acc) =>
          switch (bd) {
          | [] => acc
          | [_] => acc
          | [Toad, Toad, ...tl] =>
            selfBlockScoreCalc([Toad, ...tl], acc - 10)
          | [Frog, Frog, ...tl] =>
            selfBlockScoreCalc([Frog, ...tl], acc + 10)
          | [_, ...tl] => selfBlockScoreCalc(tl, acc)
          };

        let rec jumpScoreCalc = (bd, i, acc) =>
          switch (bd) {
          | [] => acc
          | [_] => acc
          | [Toad, Frog, Empty, ...tl] =>
            jumpScoreCalc([Frog, Empty, ...tl], i + 1, acc + 30)
          | [Frog, Toad, Empty, ...tl] =>
            jumpScoreCalc([Toad, Empty, ...tl], i + 1, acc - 30)
          | [_, ...tl] => jumpScoreCalc(tl, i + 1, acc)
          };

        float_of_int(
          legalMovesDiff
          + usedNothingDiff
          + advancementDiffCalc(s.board, 1, 0)
          + oppBlockScoreCalc(s.board, 1, 0)
          + selfBlockScoreCalc(s.board, 0),
        );
      };

  /* Check Expects for estimateValue */

  checkExpect(
    estimateValue({
      board: [Frog, Frog, Empty, Toad, Empty, Toad],
      who: P2,
      usedNothingP1: true,
      usedNothingP2: true,
    }),
    1000.0,
    "Success!",
  );

  checkExpect(
    estimateValue({
      board: [Frog, Frog, Empty, Empty, Toad, Toad],
      who: P1,
      usedNothingP1: true,
      usedNothingP2: true,
    }),
    -1000.0,
    "Success!",
  );
};

module MyGame: Game = TNF;
open TNF /* When # of toads is even and # of empty is even -> P*/ /* When # of toads is even and # of empty is odd -> P*/;
// + jumpScoreCalc(s.board, 1, 0),+ centerScoreCalc(s.board, 1, 0)
/* Your test cases go here! */
