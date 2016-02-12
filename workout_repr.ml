open Batteries

let from_lexbuf = Parser.workout Lexer.read

let from_channel = from_lexbuf % Lexing.from_channel

let from_string = from_lexbuf % Lexing.from_string

module Ir = struct
  let int32_caps =
    ((List.fold_left Int32.add Int32.zero) %
     (List.map Workout.Capability.to_int32)) %
    Workout.caps

  let capabilities = Int32.to_int % int32_caps

  let num_valid_steps {Workout.steps; _} =
    List.length (Non_empty_list.to_list steps)

  let to_channel _chan _w =
    ()
end
