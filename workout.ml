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

module Heart_rate = struct
  type t = Absolute of int      (* bpm *)
         | Percent of int       (* 0 - 100 % of max. *)
end

module Power = struct
  type t = Absolute of int      (* W *)
         | Percent of int       (* 0 - 1000 % of FTP *)
end

module Repeat_cond = struct
  type t = Times of int
         | Time_lt of int     (* ms *)
         | Distance_lt of int (* cm *)
         | Calories_lt of int (* kcal *)
         | Heart_rate_lt of Heart_rate.t
         | Heart_rate_gt of Heart_rate.t
         | Power_lt of Power.t
         | Power_gt of Power.t
end

module Duration = struct
  type t = Time_lt of int       (* ms *)
         | Distance_lt of int   (* cm *)
         | Heart_rate_lt of Heart_rate.t
         | Heart_rate_gt of Heart_rate.t
         | Calories_lt of int   (* kcal *)
         | Power_lt of Power.t
         | Power_gt of Power.t
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
    duration : Duration.t option;
    target : Target.t option;
    intensity : Intensity.t option;
  }

  type t = Step of step
         | Repeat of Repeat_cond.t * (t Non_empty_list.t)
end

type t = {
  name : string option;
  sport : Sport.t option;
  steps : Step.t Non_empty_list.t;
}
