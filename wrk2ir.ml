let () =
  Lexing.from_channel stdin |>
  Parser.parse Lexer.read |>
  print_endline
