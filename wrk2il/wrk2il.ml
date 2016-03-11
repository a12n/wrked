open Batteries

let parse_args () =
  let name = ref None in
  let sport = ref None in
  Arg.parse [ "-name", Arg.String (Ref.set name % Option.some),
              " Override workout name";
              "-sport", Arg.Symbol ([ "cycling"; "running";
                                      "swimming"; "walking" ],
                                     (Ref.set sport % Option.some %
                                      Workout.Sport.of_string)),
              " Override workout sport" ]
    (fun _anon -> ())
    "Translate workout description to intermediate language";
  !name, !sport

let override ?name ?sport =
  (fun workout ->
     if Option.is_some name then
       {workout with Workout.name}
     else workout) %
  (fun workout ->
     if Option.is_some sport then
       {workout with Workout.sport}
     else workout)

let () =
  let name, sport = parse_args () in
  let lexbuf = Lexing.from_channel stdin in
  try
    Repr.from_lexbuf lexbuf |>
    override ?name ?sport |>
    Repr.Il.to_channel stdout
  with Lexer.Error | Parser.Error ->
    Lexing.(
      let {pos_lnum; pos_bol; pos_cnum; _} = lexeme_start_p lexbuf in
      Printf.eprintf "Error near \"%s\" at line %d, column %d\n"
        (lexeme lexbuf) pos_lnum (pos_cnum - pos_bol)
    )
