module Sport : sig
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

module Speed : sig
  type t = private float        (* m/s *)
  type zone = private int

  val of_float : float -> t
  val zone_of_int : int -> zone
end

module Cadence : sig
  type t = private int          (* rpm *)
  type zone = private int

  val of_int : int -> t
  val zone_of_int : int -> zone
end

module Heart_rate : sig
  type absolute = private int   (* bpm *)
  type percent = private int    (* 0-100 % of max *)
  type zone = private int       (* 1-5 *)

  val absolute_of_int : int -> absolute
  val percent_of_int : int -> percent
  val zone_of_int : int -> zone

  type t = Absolute of absolute
         | Percent of percent
end

module Power : sig
  type absolute = private int   (* W *)
  type percent = private int    (* 0-1000 % of FTP *)
  type zone = private int       (* 1-7 *)

  val absolute_of_int : int -> absolute
  val percent_of_int : int -> percent
  val zone_of_int : int -> zone

  type t = Absolute of absolute
         | Percent of percent
end

module Condition : sig
  type order = Less | Greater

  type calories = private int   (* kcal *)
  type distance = private int   (* m *)
  type time = private int       (* s *)

  val calories_of_int : int -> calories
  val distance_of_int : int -> distance
  val time_of_int : int -> time

  type t = Time of time
         | Distance of distance
         | Heart_rate of (order * Heart_rate.t)
         | Calories of calories
         | Power of (order * Power.t)
end

module Repeat : sig
  type times = private int

  val times_of_int : int -> times

  type t = Times of times
         | Until of Condition.t
end

module Target : sig
  module Value : functor (S : sig type t type zone end) ->
  sig
    type t = Zone of S.zone
           | Range of S.t * S.t
  end

  module Cadence_value : module type of Value (Cadence)
  module Heart_rate_value : module type of Value (Heart_rate)
  module Power_value : module type of Value (Power)
  module Speed_value : module type of Value (Speed)

  type t = Speed of Speed_value.t
         | Heart_rate of Heart_rate_value.t
         | Cadence of Cadence_value.t
         | Power of Power_value.t
end

module Intensity : sig
  type t = Warm_up
         | Active
         | Rest
         | Cool_down
end

module Step : sig
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

(* {2 Capabilities flags} *)

module Capability : sig
  type t = Speed
         | Heart_rate
         | Distance
         | Cadence
         | Power
         | Grade
         | Resistance

  val to_int32 : t -> int32
end

val caps : t -> Capability.t list
