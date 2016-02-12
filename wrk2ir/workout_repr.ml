open Batteries

let from_lexbuf = Parser.workout Lexer.read

let from_channel = from_lexbuf % Lexing.from_channel

let from_string = from_lexbuf % Lexing.from_string

let print_name chan name =
  Printf.fprintf chan "\"%s\": " name

let print_intensity chan intensity =
  IO.nwrite chan Workout.Intensity.(
      match intensity with
        Warm_up   -> "warm up"
      | Active    -> "active"
      | Rest      -> "rest"
      | Cool_down -> "cool down")

let print_sport chan sport =
  IO.nwrite chan Workout.Sport.(
      match sport with
        Cycling  -> "cycling"
      | Running  -> "running"
      | Swimming -> "swimming"
      | Walking  -> "walking")

let print_condition chan _condition = ()

let print_target chan _target = ()

let print_single_step chan
    {Workout.Step.name; duration; target; intensity} =
  Option.may (print_name chan) name;
  Option.may (print_intensity chan) intensity;
  match duration, target with
    None, None -> IO.nwrite chan "open"
  | Some c, None -> print_condition chan c
  | None, Some t -> print_target chan t
  | Some c, Some t ->
    (print_condition chan c;
     IO.nwrite chan ", ";
     print_target chan t)

let print_step chan = function
    Workout.Step.Single s -> ()
  | Workout.Step.Repeat r -> ()

let to_channel chan {Workout.name; sport; steps} =
  Option.may (print_name chan) name;
  Option.may (print_sport chan) sport;
  IO.write chan '[';
  List.iter (print_step chan) (Non_empty_list.to_list steps);
  IO.write chan ']'

let to_string w =
  let chan = IO.output_string () in
  to_channel chan w;
  IO.close_out chan

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
