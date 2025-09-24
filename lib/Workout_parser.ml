open Angstrom

let quoted_string = char '"' *> take_while (( <> ) '"') <* char '"'

(* let step = take_while (fun d -> d >= '0' && d <= '9') *)
(* let step_list = char '[' *> both step (many (char ';' *> step)) <* char ']' *)
let is_digit = function '0' .. '9' -> true | _ -> false

let is_space = function
  | ' ' | '\x0c' | '\n' | '\r' | '\t' | '\x0b' -> true
  | _ -> false

let lwsp = skip_while is_space
let int = lift int_of_string (take_while1 is_digit)

let float =
  lift2
    (fun a b -> float_of_string (a ^ "." ^ b))
    (take_while1 is_digit)
    (char '.' *> take_while1 is_digit)

let number = float <|> (float_of_int <$> int)

(* Time units/suffixes. *)
let unit_h = lwsp *> string_ci "h"
let unit_min = lwsp *> string_ci "min"
let unit_s = lwsp *> option "s" (string_ci "s")

(* Speed units/suffixes. *)
let unit_kmph = lwsp *> option "km/h" (string_ci "km/h")
let unit_mps = lwsp *> string_ci "m/s"

(* Cadence units. *)
let unit_rpm = lwsp *> option "rpm" (string_ci "rpm")

(* Heart rate and power units. *)
let unit_bpm = lwsp *> option "bpm" (string_ci "bpm")
let unit_pct = lwsp *> char '%'
let unit_w = lwsp *> option "W" (string_ci "W")

(* Distance units. *)
let unit_km = lwsp *> string_ci "km"
let unit_m = lwsp *> option "m" (string_ci "m")

(* Calories units. *)
let unit_kcal = lwsp *> option "kcal" (string_ci "kcal")

module Sport = struct
  let cycling =
    lwsp
    *> Workout.Sport.(
         string_ci "spin" *> return Spin
         <|> string_ci "indoor" *> return (Indoor : cycling)
         <|> string_ci "road" *> return Road
         <|> string_ci "mountain" *> return Mountain
         <|> string_ci "downhill" *> return Downhill
         <|> string_ci "recumbent" *> return Recumbent
         <|> string_ci "cyclocross" *> return Cyclocross
         <|> string_ci "hand" *> return Hand
         <|> string_ci "track" *> return (Track : cycling)
         <|> string_ci "bmx" *> return BMX
         <|> string_ci "gravel" *> return Gravel
         <|> string_ci "commuting" *> return Commuting
         <|> string_ci "mixed_surface" *> return Mixed_surface)

  let running =
    lwsp
    *> Workout.Sport.(
         string_ci "treadmill" *> return Treadmill
         <|> string_ci "street" *> return Street
         <|> string_ci "trail" *> return Trail
         <|> string_ci "track" *> return (Track : running)
         <|> string_ci "indoor" *> return (Indoor : running))

  let swimming =
    lwsp
    *> Workout.Sport.(
         string_ci "lap" *> return Lap
         <|> string_ci "open_water" *> return Open_water)

  let t =
    let cycling_sport =
      let* c =
        string_ci "cycling"
        *> option None (lwsp *> char '/' *> cycling >>| Option.some)
      in
      return (Workout.Sport.Cycling c)
    in
    let running_sport =
      let* r =
        string_ci "running"
        *> option None (lwsp *> char '/' *> running >>| Option.some)
      in
      return (Workout.Sport.Running r)
    in
    let swimming_sport =
      let* s =
        string_ci "swimming"
        *> option None (lwsp *> char '/' *> swimming >>| Option.some)
      in
      return (Workout.Sport.Swimming s)
    in
    lwsp *> (cycling_sport <|> running_sport <|> swimming_sport)
end

module Speed = struct
  let zone = lwsp *> int >>| Workout.Speed.zone_of_int

  let t =
    lwsp
    *> lift Workout.Speed.of_float
         (number <* unit_mps
         <|> (number <* unit_kmph >>| ( *. ) (1000.0 /. 3600.0)))
end

module Cadence = struct
  let zone = lwsp *> int >>| Workout.Cadence.zone_of_int
  let t = lwsp *> int <* unit_rpm >>| Workout.Cadence.of_int
end

module Heart_rate = struct
  let zone = lwsp *> int >>| Workout.Heart_rate.zone_of_int

  let t =
    lwsp
    *> (int <* unit_pct >>| Workout.Heart_rate.of_int_relative
       <|> (int <* unit_bpm >>| Workout.Heart_rate.of_int))
end

module Power = struct
  let zone = lwsp *> int >>| Workout.Power.zone_of_int

  let t =
    lwsp
    *> (int <* unit_pct >>| Workout.Power.of_int_relative
       <|> (int <* unit_w >>| Workout.Power.of_int))
end

module Time = struct
  let t =
    lift3
      (fun h min s ->
        let s_h = 3600.0 *. h |> Float.round |> Float.to_int in
        let s_min = 60.0 *. min |> Float.round |> Float.to_int in
        Workout.Time.of_int (s_h + s_min + s))
      (option 0.0 (lwsp *> number <* unit_h))
      (option 0.0 (lwsp *> number <* unit_min))
      (option 0 (lwsp *> int <* unit_s))
end

module Distance = struct
  let t =
    lwsp *> (int <* unit_km >>| ( * ) 1000 <|> (int <* unit_m))
    >>| Workout.Distance.of_int
end

module Calories = struct
  let t = lwsp *> int <* unit_kcal >>| Workout.Calories.of_int
end

module Condition = struct
  let relation =
    Workout.Condition.(
      lwsp *> (char '<' *> return Less <|> char '>' *> return Greater))

  let t =
    let time_condition =
      string_ci "time" *> lwsp *> Time.t >>| fun t -> Workout.Condition.Time t
    in
    let distance_condition =
      string_ci "distance" *> lwsp *> Distance.t >>| fun d ->
      Workout.Condition.Distance d
    in
    let heart_rate_condition =
      string_ci "hr" *> lwsp *> both relation Heart_rate.t >>| fun h ->
      Workout.Condition.Heart_rate h
    in
    let calories_condition =
      string_ci "calories" *> lwsp *> Calories.t >>| fun c ->
      Workout.Condition.Calories c
    in
    let power_condition =
      string_ci "power" *> lwsp *> both relation Power.t >>| fun p ->
      Workout.Condition.Power p
    in
    lwsp
    *> (time_condition <|> distance_condition <|> heart_rate_condition
      <|> calories_condition <|> power_condition)
end

module Repeat = struct
  let times =
    lift Workout.Repeat.times_of_int
      (int <* (char 'x' <|> char 'X' <|> char '*'))

  let t =
    let times_repeat = lwsp *> times >>| fun t -> Workout.Repeat.Times t in
    let until_repeat = Condition.t >>| fun c -> Workout.Repeat.Until c in
    times_repeat <|> until_repeat
end

module Target = struct
  module Cadence = struct
    let t =
      let zone =
        lwsp *> string_ci "zone" *> Cadence.zone >>| fun z ->
        Workout.Target.Cadence.Zone z
      in
      let range =
        both Cadence.t (lwsp *> char '-' *> Cadence.t)
        >>| Workout.Target.Cadence.range_of_pair
        >>| fun r -> Workout.Target.Cadence.Range r
      in
      string_ci "cadence" *> (zone <|> range) >>| fun c ->
      Workout.Target.Cadence c
  end

  module Heart_rate = struct
    let t =
      let zone =
        lwsp *> string_ci "zone" *> Heart_rate.zone >>| fun z ->
        Workout.Target.Heart_rate.Zone z
      in
      let range =
        both Heart_rate.t (lwsp *> char '-' *> Heart_rate.t)
        >>| Workout.Target.Heart_rate.range_of_pair
        >>| fun r -> Workout.Target.Heart_rate.Range r
      in
      string_ci "hr" *> (zone <|> range) >>| fun h ->
      Workout.Target.Heart_rate h
  end

  module Power = struct
    let t =
      let zone =
        lwsp *> string_ci "zone" *> Power.zone >>| fun z ->
        Workout.Target.Power.Zone z
      in
      let range =
        both Power.t (lwsp *> char '-' *> Power.t)
        >>| Workout.Target.Power.range_of_pair
        >>| fun r -> Workout.Target.Power.Range r
      in
      string_ci "power" *> (zone <|> range) >>| fun p -> Workout.Target.Power p
  end

  module Speed = struct
    let t =
      let zone =
        lwsp *> string_ci "zone" *> Speed.zone >>| fun z ->
        Workout.Target.Speed.Zone z
      in
      let range =
        both Speed.t (lwsp *> char '-' *> Speed.t)
        >>| Workout.Target.Speed.range_of_pair
        >>| fun r -> Workout.Target.Speed.Range r
      in
      string_ci "speed" *> (zone <|> range) >>| fun s -> Workout.Target.Speed s
  end

  let t = lwsp *> (Speed.t <|> Heart_rate.t <|> Cadence.t <|> Power.t)
end

module Step = struct
  let intensity =
    lwsp
    *> Workout.Step.(
         string_ci "active" *> return Active
         <|> string_ci "rest" *> return Rest
         <|> string_ci "warmup" *> return Warmup
         <|> string_ci "cooldown" *> return Cooldown
         <|> string_ci "recovery" *> return Recovery
         <|> string_ci "interval" *> return Interval
         <|> string_ci "other" *> return Other)

  let single =
    lwsp
    *> lift3
         (fun name intensity (duration, target) ->
           Workout.Step.{ name; descr = None; duration; target; intensity })
         (option None
            (lwsp *> quoted_string <* lwsp <* char ':' >>| Option.some))
         (option None (intensity <* lwsp <* char ',' >>| Option.some))
         (lwsp
         *> (string_ci "open"
            >>| (fun _ -> (None, None))
            <|> ( both Condition.t (lwsp *> char ',' *> Target.t)
                >>| fun (c, t) -> (Some c, Some t) )
            <|> (Condition.t >>| fun c -> (Some c, None))
            <|> (Target.t >>| fun t -> (None, Some t))))
end

let t =
  let consume = Consume.All in
  parse_string ~consume (Step.single <* lwsp <* end_of_input)
