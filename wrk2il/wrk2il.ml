open Batteries

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    Workout_repr.from_lexbuf lexbuf |>
    Workout_repr.Il.to_channel stdout
  with Lexer.Error | Parser.Error ->
    Lexing.(
      let {pos_lnum; pos_bol; pos_cnum; _} = lexeme_start_p lexbuf in
      Printf.eprintf "Error near \"%s\" at line %d, column %d\n"
        (lexeme lexbuf) pos_lnum (pos_cnum - pos_bol)
    )
