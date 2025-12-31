/* take in a string, s, and print it with green color */
let printGreen = (s: string): unit =>
  print_string("\027[32m" ++ s ++ "\027[0m\n");

/* take in a string, s, and print it with red color */
let printRed = (s: string): unit =>
  print_string("\027[31m" ++ s ++ "\027[0m\n");

type result('a) =
  | Actual_Result('a)
  | Expected_Result('a)
  | Actual_Error(string)
  | Expected_Error(string);

type check_result('a) =
  | Test_Passed
  | Test_Failed(result('a), result('a));

/* checkExpect
   Inputs: actual and expected, two 'a and message, a string
   Output: nothing */
let checkExpect = (actual: 'a, expected: 'a, message: string): unit =>
  if (actual == expected) {
    printGreen("checkExpectSuccess: " ++ message);
  } else {
    printRed("checkExpectFail: " ++ message);
    printRed("expected output: ");
    Js.log(expected);
    printRed("actual output: ");
    Js.log(actual);
  };

/* checkError
   Input: a one-argument procedure 'input' that you want to test
   and a string of the error message of the 'failwith' clause in the procedure
   Output: nothing */
let checkError = (input: unit => 'a, expect: string): unit =>
  try(
    {
      ignore(input()); // we expect this to error
      failwith("Error did not occur");
    }
  ) {
  | Failure(err) when err == expect => printGreen("checkErrorSuccess")
  | Failure(err) when err == "Error did not occur" =>
    printRed("Error did not occur")
  | Failure(err) =>
    printRed(
      "checkErrorFail. Expected error: " ++ expect ++ "; Actual error: " ++ err,
    )
  };

/* parseInput
      Input: a string consisting of only the characters 'T', 'F', or '.'
      Output: a list of ints where 1 represents 'T', 2 represents 'F', and 0
        represents '.', in the same order as the original input.
   */
let parseInput: string => list(int) =
  s => {
    let indices: int => list(int) =
      n => {
        let rec indicesHelper: int => list(int) =
          n =>
            if (n <= 0) {
              [];
            } else {
              [n - 1, ...indicesHelper(n - 1)];
            };
        List.rev(indicesHelper(n));
      };
    let f =
      fun
      | '.' => 0
      | 'T' => 1
      | 'F' => 2
      | _ => failwith("invalid character in initialization string");

    let convertStringToIntList: string => list(int) =
      s => List.map(i => f(s.[i]), indices(String.length(s)));

    convertStringToIntList(s);
  };
