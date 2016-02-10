open Batteries

let from_lexbuf = Parser.workout Lexer.read

let from_channel chan = from_lexbuf (Lexing.from_channel chan)

module Ir = struct
  let int32_caps =
    ((List.fold_left Int32.add Int32.zero) %
     (List.map Workout.Capability.to_int32)) %
    Workout.caps

  let capabilities = Int32.to_int % int32_caps

  let num_valid_steps {Workout.steps; _} =
    List.length (Non_empty_list.to_list steps)

  let to_channel chan wrk =
    let s str =
      output_string chan str;
      output_char chan '\n' in
    let i = s % string_of_int in
    s "file_id";
    s "end_file_id";
    s "workout";
    s "num_valid_steps";
    i (num_valid_steps wrk);
    s "capabilities";
    i (capabilities wrk);
    s "end_workout"
end
