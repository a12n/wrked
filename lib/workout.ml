type 'a non_empty_list = 'a * 'a list

let non_empty_list_to_list (x, xs) = x :: xs

module Capability = struct
  type t =
    | Speed
    | Heart_rate
    | Distance
    | Cadence
    | Power
    | Grade
    | Resistance

  (* Values from FIT SDK *)
  let to_int32 = function
    | Speed -> 0x00000080l
    | Heart_rate -> 0x00000100l
    | Distance -> 0x00000200l
    | Cadence -> 0x00000400l
    | Power -> 0x00000800l
    | Grade -> 0x00001000l
    | Resistance -> 0x00002000l
end

module Sport = struct
  module Cycling = struct
    type t =
      | Spin
      | Indoor
      | Road
      | Mountain
      | Downhill
      | Recumbent
      | Cyclocross
      | Hand
      | Track
      | BMX
      | Gravel
      | Commuting
      | Mixed_Surface
  end

  module Running = struct
    type t = Treadmill | Street | Trail | Track | Indoor
  end

  module Swimming = struct
    type t = Lap | Open_Water
  end

  type t =
    | Cycling of Cycling.t option
    | Running of Running.t option
    | Swimming of Swimming.t option

  let of_string = function
    | "cycling" -> Cycling None
    | "running" -> Running None
    | "swimming" -> Swimming None
    | _ -> invalid_arg "Workout.Sport.of_string"

  let to_string = function
    | Cycling _ -> "cycling"
    | Running _ -> "running"
    | Swimming _ -> "swimming"
end

let restricted (min, max) exn = function
  | n when n >= min && n <= max -> n
  | _ -> raise exn

module Speed = struct
  type t = float
  type zone = int

  let of_float =
    restricted (1.0, 100.0) (Invalid_argument "Workout.Speed.of_float")

  let zone_of_int =
    restricted (1, 10) (Invalid_argument "Workout.Speed.zone_of_int")

  let of_float_kmph x = x *. 1000.0 /. 3600.0
end

module Cadence = struct
  type t = int
  type zone = int

  let of_int = restricted (1, 500) (Invalid_argument "Workout.Cadence.of_int")

  (* TODO: Max cadence zone? *)
  let zone_of_int =
    restricted (1, 10) (Invalid_argument "Workout.Cadence.zone_of_int")
end

module Heart_rate = struct
  type absolute = int
  type relative = int
  type zone = int

  let absolute_of_int =
    restricted (1, 255) (Invalid_argument "Workout.Heart_rate.absolute_of_int")

  let relative_of_int =
    restricted (1, 100) (Invalid_argument "Workout.Heart_rate.relative_of_int")

  let zone_of_int =
    restricted (1, 5) (Invalid_argument "Workout.Heart_rate.zone_of_int")

  type t = Absolute of absolute | Relative of relative
end

module Power = struct
  type absolute = int
  type relative = int
  type zone = int

  let absolute_of_int =
    restricted (1, 10000) (Invalid_argument "Workout.Power.absolute_of_int")

  let relative_of_int =
    restricted (1, 1000) (Invalid_argument "Workout.Power.relative_of_int")

  let zone_of_int =
    restricted (1, 7) (Invalid_argument "Workout.Power.zone_of_int")

  type t = Absolute of absolute | Relative of relative
end

module Condition = struct
  type order = Less | Greater
  type calories = int
  type distance = int
  type time = int

  let calories_of_int =
    restricted (1, max_int)
      (Invalid_argument "Workout.Condition.calories_of_int")

  let distance_of_int =
    restricted (1, max_int)
      (Invalid_argument "Workout.Condition.distance_of_int")

  let time_of_int =
    restricted (1, max_int) (Invalid_argument "Workout.Condition.time_of_int")

  type t =
    | Time of time
    | Distance of distance
    | Heart_rate of (order * Heart_rate.t)
    | Calories of calories
    | Power of (order * Power.t)

  let caps = function
    | Distance _ -> [ Capability.Distance ]
    | Heart_rate _ -> [ Capability.Heart_rate ]
    | Power _ -> [ Capability.Power ]
    | Calories _ | Time _ -> []
end

module Repeat = struct
  type times = int

  let times_of_int =
    restricted (2, 1000000) (Invalid_argument "Workout.Repeat.times_of_int")

  type t = Times of times | Until of Condition.t

  let caps = function Times _ -> [] | Until c -> Condition.caps c
end

module Target = struct
  module Value (S : sig
    type t
    type zone
  end) =
  struct
    type range = S.t * S.t
    type t = Zone of S.zone | Range of range

    let range_of_pair (a, b) = if a < b then (a, b) else (b, a)
  end

  module Cadence_value = Value (Cadence)
  module Heart_rate_value = Value (Heart_rate)
  module Power_value = Value (Power)
  module Speed_value = Value (Speed)

  type t =
    | Speed of Speed_value.t
    | Heart_rate of Heart_rate_value.t
    | Cadence of Cadence_value.t
    | Power of Power_value.t

  let caps = function
    | Speed _ -> [ Capability.Speed ]
    | Heart_rate _ -> [ Capability.Heart_rate ]
    | Cadence _ -> [ Capability.Cadence ]
    | Power _ -> [ Capability.Power ]
end

module Intensity = struct
  type t = Active | Rest | Warmup | Cooldown | Recovery | Interval | Other

  let of_string = function
    | "active" -> Active
    | "rest" -> Rest
    | "warmup" -> Warmup
    | "cooldown" -> Cooldown
    | "recovery" -> Rest
    | "interval" -> Active
    | "other" -> Other
    | _ -> invalid_arg "Workout.Intensity.of_string"

  let to_string = function
    | Active -> "active"
    | Rest -> "rest"
    | Warmup -> "warmup"
    | Cooldown -> "cooldown"
    | Recovery -> "recovery"
    | Interval -> "interval"
    | Other -> "other"
end

module Step = struct
  type single = {
    name : string option;
    duration : Condition.t option;
    target : Target.t option;
    intensity : Intensity.t option;
  }

  and repeat = { condition : Repeat.t; steps : t non_empty_list }
  and t = Single of single | Repeat of repeat

  let rec caps = function
    | Single { duration; target; _ } ->
        List.append
          (match duration with
          | Some duration -> Condition.caps duration
          | None -> [])
          (match target with Some target -> Target.caps target | None -> [])
    | Repeat { condition; steps } ->
        List.append (Repeat.caps condition)
          (non_empty_list_to_list steps |> List.map caps |> List.flatten)
end

type t = {
  name : string option;
  sport : Sport.t option;
  steps : Step.t non_empty_list;
}

let caps { steps; _ } =
  non_empty_list_to_list steps
  |> List.map Step.caps |> List.flatten |> List.sort_uniq compare
