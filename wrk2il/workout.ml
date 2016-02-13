open Batteries

module Capability = struct
  type t = Speed
         | Heart_rate
         | Distance
         | Cadence
         | Power
         | Grade
         | Resistance

  (* Values from FIT SDK *)
  let to_int32 = function
      Speed      -> 0x00000080l
    | Heart_rate -> 0x00000100l
    | Distance   -> 0x00000200l
    | Cadence    -> 0x00000400l
    | Power      -> 0x00000800l
    | Grade      -> 0x00001000l
    | Resistance -> 0x00002000l
end

module Sport = struct
  type t = Cycling
         | Running
         | Swimming
         | Walking

  let of_string = function
      "cycling"  -> Cycling
    | "running"  -> Running
    | "swimming" -> Swimming
    | "walking"  -> Walking
    | _ -> raise (Invalid_argument "Workout.Sport.of_string")

  let to_string = function
      Cycling  -> "cycling"
    | Running  -> "running"
    | Swimming -> "swimming"
    | Walking  -> "walking"
end

let restricted (min, max) exn =
  function n when n >= min && n <= max -> n
         | _ -> raise exn

module Speed = struct
  type t = float
  type zone = int

  let of_float = restricted (0.0, 100.0)
      (Invalid_argument "Workout2.Speed.of_float")

  let zone_of_int = restricted (1, 10)
      (Invalid_argument "Workout2.Speed.zone_of_int")

  let from_kmph x = x *. 1000.0 /. 3600.0
end

module Cadence = struct
  type t = int
  type zone = int

  let of_int = restricted (0, 500)
      (Invalid_argument "Workout2.Cadence.of_int")

  (* TODO: Max cadence zone? *)
  let zone_of_int = restricted (1, 10)
      (Invalid_argument "Workout2.Cadence.zone_of_int")
end

module Heart_rate = struct
  type absolute = int
  type percent = int
  type zone = int

  let absolute_of_int = restricted (0, 255)
      (Invalid_argument "Workout.Heart_rate.absolute_of_int")

  let percent_of_int = restricted (0, 100)
      (Invalid_argument "Workout.Heart_rate.percent_of_int")

  let zone_of_int = restricted (1, 5)
      (Invalid_argument "Workout.Heart_rate.zone_of_int")

  type t = Absolute of absolute
         | Percent of percent
end

module Power = struct
  type absolute = int
  type percent = int
  type zone = int

  let absolute_of_int = restricted (0, 10000)
      (Invalid_argument "Workout.Power.absolute_of_int")

  let percent_of_int = restricted (0, 1000)
      (Invalid_argument "Workout.Power.percent_of_int")

  let zone_of_int = restricted (1, 7)
      (Invalid_argument "Workout.Power.zone_of_int")

  type t = Absolute of absolute
         | Percent of percent
end

module Condition = struct
  type order = Less | Greater

  type calories = int
  type distance = int
  type time = int

  let calories_of_int = restricted (0, max_int)
      (Invalid_argument "Workout.Condition.calories_of_int")

  let distance_of_int = restricted (0, max_int)
      (Invalid_argument "Workout.Condition.distance_of_int")

  let time_of_int = restricted (0, max_int)
      (Invalid_argument "Workout.Condition.time_of_int")

  type t = Time of time
         | Distance of distance
         | Heart_rate of (order * Heart_rate.t)
         | Calories of calories
         | Power of (order * Power.t)

  let caps = function
    | Distance _ -> [Capability.Distance]
    | Heart_rate _ -> [Capability.Heart_rate]
    | Power _ -> [Capability.Power]
    | Calories _ | Time _ -> []
end

module Repeat = struct
  type times = int

  let times_of_int = restricted (2, 1000000)
      (Invalid_argument "Workout.Repeat.times_of_int")

  type t = Times of times
         | Until of Condition.t

  let caps = function
      Times _ -> []
    | Until c -> Condition.caps c
end

module Target = struct
  module Value (S : sig type t type zone end) = struct
    type range = (S.t * S.t)

    type t = Zone of S.zone
           | Range of range

    let range_of_pair (a, b) = if a < b then (a, b) else (b, a)
  end

  module Cadence_value = Value (Cadence)
  module Heart_rate_value = Value (Heart_rate)
  module Power_value = Value (Power)
  module Speed_value = Value (Speed)

  type t = Speed of Speed_value.t
         | Heart_rate of Heart_rate_value.t
         | Cadence of Cadence_value.t
         | Power of Power_value.t

  let caps = function
      Speed _ -> [Capability.Speed]
    | Heart_rate _ -> [Capability.Heart_rate]
    | Cadence _ -> [Capability.Cadence]
    | Power _ -> [Capability.Power]
end

module Intensity = struct
  type t = Warm_up
         | Active
         | Rest
         | Cool_down
end

module Step = struct
  type single = {
    name      : string option;
    duration  : Condition.t option;
    target    : Target.t option;
    intensity : Intensity.t option;
  } and repeat = {
    condition : Repeat.t;
    steps     : t Non_empty_list.t;
  } and t = Single of single
          | Repeat of repeat

  let rec caps = function
      Single {duration; target; _} ->
      List.append
        (Option.map_default Condition.caps [] duration)
        (Option.map_default Target.caps [] target)
    | Repeat {condition; steps} ->
      List.append
        (Repeat.caps condition)
        (Non_empty_list.to_list steps |>
         List.map caps |> List.flatten)
end

type t = {
  name  : string option;
  sport : Sport.t option;
  steps : Step.t Non_empty_list.t;
}

let caps {steps; _} =
  Non_empty_list.to_list steps |>
  List.map Step.caps |> List.flatten |>
  List.sort_uniq compare
