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

  let to_string = function
      Cycling -> "cycling"
    | _ -> "generic"
end

module Heart_rate = struct
  type t = Absolute of int      (* bpm *)
         | Percent of int       (* 0 - 100 % of max. *)
end

module Power = struct
  type t = Absolute of int      (* W *)
         | Percent of int       (* 0 - 1000 % of FTP *)
end

module Condition = struct
  type order = Less | Greater
  type t = Time of int       (* ms *)
         | Distance of int   (* cm *)
         | Heart_rate of order * Heart_rate.t
         | Calories of int   (* kcal *)
         | Power of order * Power.t
end

module Repeat_condition = struct
  type t = Times of int
         | Condition of Condition.t
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
  type step = {
    name : string option;
    duration : Condition.t option;
    target : Target.t option;
    intensity : Intensity.t option;
  }

  type t = Single of step
         | Repeat of Repeat_condition.t * (t Non_empty_list.t)
end

type t = {
  name : string option;
  sport : Sport.t option;
  steps : Step.t Non_empty_list.t;
}

let print wrk =
  print_endline "workout";
  (* TODO *)
  print_endline "end_workout"
