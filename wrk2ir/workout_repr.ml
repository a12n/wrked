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

  open Workout

  let int_of_heart_rate = function
      Heart_rate.Absolute bpm    -> (bpm :> int) + 100
    | Heart_rate.Percent percent -> (percent :> int)

  let int_of_power = function
      Power.Absolute w      -> (w :> int) + 1000
    | Power.Percent percent -> (percent :> int)

  let p_line ch line = IO.(nwrite ch line; write ch '\n')

  let p_field ch k v = p_line ch k; p_line ch v

  let p_int_field ch k v = p_field ch k (string_of_int v)

  let p_float_field ch k = p_field ch k % string_of_float

  let p_duration ch cond =
    let duration_type, field_name, value =
      Condition.(
        match cond with
          Time s ->
          "time",
          "duration_time",
          (s :> int)
        | Distance m ->
          "distance",
          "duration_distance",
          (m :> int)
        | Calories kcal ->
          "colories",
          "duration_calories",
          (kcal :> int)
        | Heart_rate (Less, hr) ->
          "hr_less_than",
          "duration_hr",
          (int_of_heart_rate hr)
        | Heart_rate (Greater, hr) ->
          "hr_greater_than",
          "duration_hr",
          (int_of_heart_rate hr)
        | Power (Less, power) ->
          "power_less_than",
          "duration_power",
          (int_of_power power)
        | Power (Greater, power) ->
          "power_greater_than",
          "duration_power",
          (int_of_power power)
      ) in
    p_field ch "duration_type" duration_type;
    p_int_field ch field_name value

  let p_duration_opt ch = function
      None      -> p_field ch "duration_type" "open" (* TODO: duration_value? *)
    | Some cond -> p_duration ch cond

  let p_target ch target =
    (* TODO *)
    match target with
    | Target.Speed (Target.Speed_value.Zone z) -> (
        p_field ch "target_type" "speed";
        p_int_field ch "target_value" (z :> int)
      )
    | Target.Speed (Target.Speed_value.Range r) -> (
        p_field ch "target_type" "speed";
        let a, b = (r :> (Speed.t * Speed.t)) in
        p_float_field ch "custom_target_speed_low" (a :> float);
        p_float_field ch "custom_target_speed_high" (b :> float)
      )

  let p_target_opt ch = function
      None        -> p_field ch "target_type" "open"
    | Some target -> p_target ch target

  let p_single_step ch {Step.name; duration; target; intensity} =
    p_line ch "workout_step";
    Option.may (p_field ch "name") name;
    p_duration_opt ch duration;
    p_target_opt ch target;
    Option.may (p_field ch "intensity" % Intensity.(
        function Warm_up   -> "warmup"
               | Active    -> "active"
               | Rest      -> "rest"
               | Cool_down -> "cooldown")) intensity;
    p_line ch "end_workout_step"

  let rec p_step ch i = function
      Step.Single s -> p_single_step ch s; i + 1
    | Step.Repeat r -> p_repeat_step ch i r
  and p_repeat_step ch i {Step.condition; steps} =
    let k = List.fold_left (p_step ch) i
        (Non_empty_list.to_list steps) in
    p_line ch "workout_step";
    p_int_field ch "duration_step" i; (* Repeat from step i *)
    (match condition with
       Repeat.Times n -> (
         p_field ch "duration_type" "repeat_until_steps_cmplt";
         p_int_field ch "repeat_steps" (n :> int)
       )
     | Repeat.Until (Condition.Time s) -> (
         p_field ch "duration_type" "repeat_until_time";
         p_int_field ch "duration_time" (s :> int)
       )
     | Repeat.Until (Condition.Distance m) -> (
         p_field ch "duration_type" "repeat_until_distance";
         p_int_field ch "duration_distance" (m :> int)
       )
     | Repeat.Until (Condition.Calories kcal) -> (
         p_field ch "duration_type" "repeat_until_calories";
         p_int_field ch "duration_calories" (kcal :> int)
       )
     | Repeat.Until (Condition.Heart_rate (order, hr)) -> (
         p_field ch "duration_type" Condition.(
             match order with
               Less    -> "repeat_until_hr_less_than"
             | Greater -> "repeat_until_hr_greater_than"
           );
         p_int_field ch "duration_hr" (int_of_heart_rate hr)
       )
     | Repeat.Until (Condition.Power (order, power)) -> (
         p_field ch "duration_type" Condition.(
             match order with
               Less    -> "repeat_until_power_less_than"
             | Greater -> "repeat_until_power_greater_than"
           );
         p_int_field ch "duration_power" (int_of_power power)
       )
    );
    p_line ch "end_workout_step";
    k + 1

  let rec n_steps = function
      [] -> 0
    | (Step.Single _) :: tail -> 1 + (n_steps tail)
    | (Step.Repeat {Step.steps; _}) :: tail ->
      1 + (n_steps (Non_empty_list.to_list steps)) + (n_steps tail)

  let to_channel ch {name; sport; steps} =
    p_line ch "workout";
    Option.may (p_field ch "name") name;
    Option.may (p_field ch "sport" % Sport.(
        function Cycling  -> "cycling"
               | Running  -> "running"
               | Swimming -> "swimming"
               | Walking  -> "walking")) sport;
    p_int_field ch "num_valid_steps"
      (n_steps (Non_empty_list.to_list steps));
    p_line ch "end_workout";
    ignore (
      List.fold_left (p_step ch) 0 (Non_empty_list.to_list steps))
end
