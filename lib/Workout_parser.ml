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
    let kmph = lwsp *> option "km/h" (string_ci "km/h") in
    let mps = lwsp *> string_ci "m/s" in
    lwsp
    *> lift Workout.Speed.of_float
         (number <* mps <|> (number <* kmph >>| ( *. ) (1000.0 /. 3600.0)))
end

module Cadence = struct
  let t =
    let rpm = lwsp *> option "rpm" (string_ci "rpm") in
    lift Workout.Cadence.of_int (lwsp *> int <* rpm)
end

module Heart_rate = struct
  let zone = lwsp *> int >>| Workout.Heart_rate.zone_of_int

  let t =
    let bpm = lwsp *> option "bpm" (string_ci "bpm") in
    let pct = lwsp *> char '%' in
    lwsp
    *> Workout.Heart_rate.(
         int <* pct >>| of_int_relative <|> (int <* bpm >>| of_int))
end

module Power = struct
  let t =
    let w = lwsp *> option "W" (string_ci "W") in
    let pct = lwsp *> char '%' in
    lwsp
    *> Workout.Power.(int <* pct >>| of_int_relative <|> (int <* w >>| of_int))
end

module Condition = struct
  let relation =
    Workout.Condition.(
      lwsp *> (char '<' *> return Less <|> char '>' *> return Greater))

  let calories =
    let kcal = lwsp *> option "kcal" (string_ci "kcal") in
    lift Workout.Calories.of_int (lwsp *> int <* kcal)

  let distance =
    let km = lwsp *> string_ci "km" in
    let m = lwsp *> option "m" (string_ci "m") in
    lift Workout.Distance.of_int
      (lwsp *> (int <* km >>| ( * ) 1000 <|> (int <* m)))

  (* FIXME: no seconds part in "1 h" *)
  (* TODO: "1.5 h" *)
  let time =
    let h = lwsp *> string_ci "h" in
    let min = lwsp *> string_ci "min" in
    let s = lwsp *> option "s" (string_ci "s") in
    lift3
      (fun h min s -> Workout.Time.of_int ((3600 * h) + (60 * min) + s))
      (option 0 (lwsp *> int <* h))
      (option 0 (lwsp *> int <* min))
      (lwsp *> int <* s)

  let condition =
    let time_condition =
      let* t = string_ci "time" *> lwsp *> time in
      return (Workout.Condition.Time t)
    in
    let distance_condition =
      let* d = string_ci "distance" *> lwsp *> distance in
      return (Workout.Condition.Distance d)
    in
    let heart_rate_condition =
      let* h = string_ci "hr" *> lwsp *> both relation Heart_rate.t in
      return (Workout.Condition.Heart_rate h)
    in
    let calories_condition =
      let* c = string_ci "calories" *> lwsp *> calories in
      return (Workout.Condition.Calories c)
    in
    let power_condition =
      let* p = string_ci "power" *> lwsp *> both relation Power.t in
      return (Workout.Condition.Power p)
    in
    lwsp
    *> (time_condition <|> distance_condition <|> heart_rate_condition
      <|> calories_condition <|> power_condition)
end

module Repeat = struct
  let times =
    lift Workout.Repeat.times_of_int
      (int <* lwsp *> (char 'x' <|> char 'X' <|> char '*'))

  let t =
    let times_repeat =
      let* t = lwsp *> times in
      return (Workout.Repeat.Times t)
    in
    let until_repeat =
      let* c = Condition.condition in
      return (Workout.Repeat.Until c)
    in
    times_repeat <|> until_repeat
end

module Target = struct
  let t =
    let hr_target =
      string_ci "hr"
      *> ((lwsp
          *>
          let* z = string_ci "zone" *> Heart_rate.zone in
          return (Workout.Target.Heart_rate (Workout.Target.Heart_rate.Zone z))
          )
         <|>
         let* r = both Heart_rate.t (lwsp *> char '-' *> Heart_rate.t) in
         return
           (Workout.Target.Heart_rate
              (Workout.Target.Heart_rate.Range
                 (Workout.Target.Heart_rate.range_of_pair r))))
    in
    let speed_target =
      let zone =
        let* z = lwsp *> string_ci "zone" *> Speed.zone in
        return (Workout.Target.Speed.Zone z)
      in
      let range =
        let* r =
          both Speed.t (lwsp *> char '-' *> Speed.t)
          >>| Workout.Target.Speed.range_of_pair
        in
        return (Workout.Target.Speed.Range r)
      in
      let* s =
        string_ci "speed" *> (zone <?> "speed zone" <|> range <?> "speed range")
      in
      return (Workout.Target.Speed s)
    in
    lwsp *> (hr_target <|> speed_target)
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
end

let t =
  let consume = Consume.All in
  parse_string ~consume (Target.t <* lwsp <* end_of_input)
