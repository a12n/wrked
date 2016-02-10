module Capability = struct
  type t = Speed
         | Heart_rate
         | Distance
         | Cadence
         | Power
         | Grade
         | Resistance
         (* ? *)
         | Interval
         | Custom
         | Fitness_equipment
         | Firstbeat
         | New_leaf
         | Tcx
         | Protected
end

module Sport = struct
  type t = Generic
         | Running
         | Cycling
         | Transition
         | Fitness_equipment
         | Swimming
         | Basketball
         | Soccer
         | Tennis
         | American_football
         | Training
         | Walking
         | Cross_country_skiing
         | Alpine_skiing
         | Snowboarding
         | Rowing
         | Mountaineering
         | Hiking
         | Multisport
         | Paddling
         | Flying
         | E_biking
         | Motorcycling
         | Boating
         | Driving
         | Golf
         | Hang_gliding
         | Horseback_riding
         | Hunting
         | Fishing
         | Inline_skating
         | Rock_climbing
         | Sailing
         | Ice_skating
         | Sky_diving
         | Snowshoeing
         | Snowmobiling
         | Stand_up_paddleboarding
         | Surfing
         | Wakeboarding
         | Water_skiing
         | Kayaking
         | Rafting
         | Windsurfing
         | Kitesurfing
end

let restricted (min, max) exn =
  function n when n >= min && n <= max -> n
         | _ -> raise exn

module Speed = struct
  type t = float                (* m/s *)
  type zone = int

  let of_float = restricted (0.0, 100.0)
      (Invalid_argument "Workout2.Speed.of_float")

  (* TODO: Max speed zone? *)
  let zone_of_int = restricted (1, 10)
      (Invalid_argument "Workout2.Speed.zone_of_int")
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

  type t = Absolute of absolute (* bpm *)
         | Percent of percent   (* 0-100 % of max *)
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

  type t = Absolute of absolute (* W *)
         | Percent of percent   (* 0-1000 % of FTP *)
end

module Condition = struct
  type order = Less | Greater

  type calories = int           (* kcal *)
  type distance = int           (* cm *)
  type time = int               (* ms *)

  let calories_of_int = restricted (0, max_int)
      (Invalid_argument "Workout.Condition.calories_of_int")

  let distance_of_int = restricted (0, max_int)
      (Invalid_argument "Workout.Condition.distance_of_int")

  let time_of_int = restricted (0, max_int)
      (Invalid_argument "Workout.Condition.time_of_int")

  type t = Time of time
         | Distance of distance
         | Heart_rate of order * Heart_rate.t
         | Calories of calories
         | Power of order * Power.t
end

module Repeat = struct
  type times = int

  let times_of_int = restricted (1, 1000000)
      (Invalid_argument "Workout.Repeat.times_of_int")

  type t = Times of times
         | Until of Condition.t
end

module Target = struct
  module Value = struct
    type t = Zone of int
           | Range of int * int
  end

  type t = Speed of Value.t
         | Heart_rate of Value.t
         | Cadence of Value.t
         | Power of Value.t
         (* | Grade of int         (\* % *\) *)
         (* | Resistance of int    (\* ? *\) *)
end

module Intensity = struct
  type t = Active
         | Cool_down
         | Rest
         | Warm_up
end

module Step = struct
  type single = {
    name      : string option;
    duration  : Condition.t option;
    target    : Target.t option;
    intensity : Intensity.t option;
  }

  type t = Single of single
         | Repeat of Repeat.t * (t Non_empty_list.t)
end

type t = {
  name  : string option;
  sport : Sport.t option;
  steps : Step.t Non_empty_list.t;
}
