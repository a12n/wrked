let () =
  match Workout.Parser.parse_channel stdin with
  | Ok w -> Workout.Printer.Channel.print stdout w
  | Error msg ->
      prerr_endline msg;
      exit 1
