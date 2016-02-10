let from_lexbuf = Parser.workout Lexer.read

let from_channel chan = from_lexbuf (Lexing.from_channel chan)

module Ir = struct
  let (%) chan str =
    output_string chan str;
    output_char chan '\n';
    chan

  let to_channel chan {Workout.steps; _} =
    chan
    % "file_id"
    % "end_file_id"
    % "workout"
    % "num_valid_steps"
    % string_of_int (List.length (Non_empty_list.to_list steps))
    % "end_workout" |> ignore
end
