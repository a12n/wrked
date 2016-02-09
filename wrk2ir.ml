let () =
  Lexing.from_channel stdin |>
  Parser.parse Lexer.read |>
  Workout.print
