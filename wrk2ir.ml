let () =
  Lexing.from_channel stdin |>
  Parser.workout Lexer.read |>
  Workout.translate |>
  print_string
