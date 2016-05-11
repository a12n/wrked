open Batteries

let from_lexbuf = Parser.workout Lexer.read

let from_channel = from_lexbuf % Lexing.from_channel

let from_string = from_lexbuf % Lexing.from_string

let heart_rate_to_channel chan hr =
  Workout.Heart_rate.(
    match hr with
      Absolute bpm    -> Printf.fprintf chan "%d" (bpm :> int)
    | Percent percent -> Printf.fprintf chan "%d%%" (percent :> int)
  )

let power_to_channel chan power =
  Workout.Power.(
    match power with
      Absolute w      -> Printf.fprintf chan "%d" (w :> int)
    | Percent percent -> Printf.fprintf chan "%d%%" (percent :> int)
  )

let condition_to_channel chan condition =
  Workout.Condition.(
    let order_to_channel = function
      | Less -> IO.write chan '<'
      | Greater -> IO.write chan '>' in
    match condition with
      Time s -> Printf.fprintf chan "time%d" (s :> int)
    | Distance m -> Printf.fprintf chan "distance%d" (m :> int)
    | Calories kcal -> Printf.fprintf chan "calories%d" (kcal :> int)
    | Heart_rate (rel, hr) ->
      (IO.nwrite chan "hr";
       order_to_channel rel;
       heart_rate_to_channel chan hr)
    | Power (rel, power) ->
      (IO.nwrite chan "power";
       order_to_channel rel;
       power_to_channel chan power)
  )

(* TODO: cleanup *)
let target_to_channel chan target =
  Workout.Target.(
    match target with
    | Speed speed ->
      (IO.nwrite chan "speed";
       match speed with
         Speed_value.Zone z ->
         Printf.fprintf chan "zone%d" (z :> int)
       | Speed_value.Range r ->
         let (a, b) = (r :> (Workout.Speed.t * Workout.Speed.t)) in
         Printf.fprintf chan "%f-%f" (a :> float) (b :> float))
    | Heart_rate hr ->
      (IO.nwrite chan "hr";
       match hr with
         Heart_rate_value.Zone z -> Printf.fprintf chan "zone%d" (z :> int)
       | Heart_rate_value.Range r ->
         let (a, b) = (r :> (Workout.Heart_rate.t * Workout.Heart_rate.t)) in
         heart_rate_to_channel chan a;
         IO.write chan '-';
         heart_rate_to_channel chan b)
    | Cadence cad ->
      (IO.nwrite chan "cadence";
       match cad with
         Cadence_value.Zone z -> Printf.fprintf chan "zone%d" (z :> int)
       | Cadence_value.Range r ->
         let (a, b) = (r :> (Workout.Cadence.t * Workout.Cadence.t)) in
         Printf.fprintf chan "%d-%d" (a :> int) (b :> int))
    | Power power ->
      (IO.nwrite chan "power";
       match power with
         Power_value.Zone z -> Printf.fprintf chan "zone%d" (z :> int)
       | Power_value.Range r ->
         let (a, b) = (r :> (Workout.Power.t * Workout.Power.t)) in
         power_to_channel chan a;
         IO.write chan '-';
         power_to_channel chan b)
  )

let repeat_to_channel chan = function
    Workout.Repeat.Times n -> Printf.fprintf chan "%dx" (n :> int)
  | Workout.Repeat.Until c -> condition_to_channel chan c

let single_step_to_channel chan
    {Workout.Step.name; duration; target; intensity} =
  Option.may (Printf.fprintf chan "\"%s\":") name;
  Option.may (fun i ->
      IO.nwrite chan (Workout.Intensity.to_string i);
      IO.write chan ',') intensity;
  match duration, target with
    None, None -> IO.nwrite chan "open"
  | Some c, None -> condition_to_channel chan c
  | None, Some t -> target_to_channel chan t
  | Some c, Some t ->
    (condition_to_channel chan c;
     IO.write chan ',';
     target_to_channel chan t)

let rec repeat_step_to_channel chan {Workout.Step.condition; steps} =
  IO.write chan '(';
  repeat_to_channel chan condition;
  IO.write chan ')';
  step_list_to_channel chan steps

and step_list_to_channel chan (s1, s) =
  IO.write chan '[';
  step_to_channel chan s1;
  List.iter (fun sn ->
      IO.write chan ';';
      step_to_channel chan sn) s;
  IO.write chan ']'

and step_to_channel chan = function
    Workout.Step.Single s -> single_step_to_channel chan s
  | Workout.Step.Repeat r -> repeat_step_to_channel chan r

let to_channel chan {Workout.name; sport; steps} =
  Option.may (Printf.fprintf chan "\"%s\":") name;
  Option.may (IO.nwrite chan % Workout.Sport.to_string) sport;
  step_list_to_channel chan steps

let to_string w =
  let chan = IO.output_string () in
  to_channel chan w;
  IO.close_out chan

module Il = struct
  open Workout

  let int32_caps =
    ((List.fold_left Int32.add Int32.zero) %
     (List.map Capability.to_int32)) % caps

  let int_of_heart_rate = function
      Heart_rate.Absolute bpm    -> (bpm :> int) + 100
    | Heart_rate.Percent percent -> (percent :> int)

  let int_of_power = function
      Power.Absolute w      -> (w :> int) + 1000
    | Power.Percent percent -> (percent :> int)

  let p_line ch line = IO.(nwrite ch line; write ch '\n')

  let p_field ch k v = p_line ch k; p_line ch v

  let p_int_field ch k = p_field ch k % string_of_int

  let p_float_field ch k = p_field ch k % string_of_float

  let p_int32_field ch k = p_field ch k % Int32.to_string

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
    Target.(
      match target with
        Speed (Speed_value.Zone z) ->
        ( p_field ch "target_type" "speed";
          (* TODO: where is target_speed_zone? *)
          p_int_field ch "target_value" (z :> int);
          p_float_field ch "custom_target_speed_low" 0.0;
          p_float_field ch "custom_target_speed_high" 0.0 )
      | Speed (Speed_value.Range r) ->
        ( p_field ch "target_type" "speed";
          let a, b = (r :> (Speed.t * Speed.t)) in
          p_int_field ch "target_value" 0;
          p_float_field ch "custom_target_speed_low" (a :> float);
          p_float_field ch "custom_target_speed_high" (b :> float) )
      | Heart_rate (Heart_rate_value.Zone z) ->
        ( p_field ch "target_type" "heart_rate";
          p_int_field ch "target_hr_zone" (z :> int);
          p_int_field ch "custom_target_heart_rate_low" 0;
          p_int_field ch "custom_target_heart_rate_high" 0 )
      | Heart_rate (Heart_rate_value.Range r) ->
        ( p_field ch "target_type" "heart_rate";
          let a, b = (r :> (Heart_rate.t * Heart_rate.t)) in
          p_int_field ch "target_hr_zone" 0;
          p_int_field ch "custom_target_heart_rate_low" (int_of_heart_rate a);
          p_int_field ch "custom_target_heart_rate_high" (int_of_heart_rate b) )
      | Cadence (Cadence_value.Zone z) ->
        ( p_field ch "target_type" "cadence";
          (* TODO: where is target_cadence_zone? *)
          p_int_field ch "target_value" (z :> int);
          p_int_field ch "custom_target_cadence_low" 0;
          p_int_field ch "custom_target_cadence_high" 0 )
      | Cadence (Cadence_value.Range r) ->
        ( p_field ch "target_type" "cadence";
          let a, b = (r :> (Cadence.t * Cadence.t)) in
          p_int_field ch "target_value" 0;
          p_int_field ch "custom_target_cadence_low" (a :> int);
          p_int_field ch "custom_target_cadence_high" (b :> int) )
      | Power (Power_value.Zone z) ->
        ( p_field ch "target_type" "power";
          p_int_field ch "target_power_zone" (z :> int);
          p_int_field ch "custom_target_power_low" 0;
          p_int_field ch "custom_target_power_high" 0 )
      | Power (Power_value.Range r) ->
        ( p_field ch "target_type" "power";
          let a, b = (r :> (Power.t * Power.t)) in
          p_int_field ch "target_power_zone" 0;
          p_int_field ch "custom_target_power_low" (int_of_power a);
          p_int_field ch "custom_target_power_high" (int_of_power b) )
    )

  let p_target_opt ch = function
      None        -> p_field ch "target_type" "open"
    | Some target -> p_target ch target

  let p_single_step ch i {Step.name; duration; target; intensity} =
    p_field ch "begin" "workout_step";
    p_int_field ch "message_index" i;
    Option.may (p_field ch "wkt_step_name") name;
    p_duration_opt ch duration;
    p_target_opt ch target;
    Option.may (p_field ch "intensity" % Intensity.to_string) intensity;
    p_field ch "end" "workout_step";
    i + 1

  let rec p_step ch i = function
      Step.Single s -> p_single_step ch i s
    | Step.Repeat r -> p_repeat_step ch i r
  and p_repeat_step ch i {Step.condition; steps} =
    let k = List.fold_left (p_step ch) i
        (Non_empty_list.to_list steps) in
    p_field ch "begin" "workout_step";
    p_int_field ch "message_index" k;
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
    p_int_field ch "duration_step" i; (* Repeat from step i *)
    p_field ch "end" "workout_step";
    k + 1

  let rec n_steps = function
      [] -> 0
    | (Step.Single _) :: tail -> 1 + (n_steps tail)
    | (Step.Repeat {Step.steps; _}) :: tail ->
      1 + (n_steps (Non_empty_list.to_list steps)) + (n_steps tail)

  let to_channel ch ({name; sport; steps} as workout) =
    p_field ch "begin" "file_id";
    p_field ch "end" "file_id";
    p_field ch "begin" "workout";
    Option.may (p_field ch "wkt_name") name;
    Option.may (p_field ch "sport" % Sport.to_string) sport;
    p_int32_field ch "capabilities" (int32_caps workout);
    p_int_field ch "num_valid_steps"
      (n_steps (Non_empty_list.to_list steps));
    p_field ch "end" "workout";
    ignore (
      List.fold_left (p_step ch) 0 (Non_empty_list.to_list steps))
end
