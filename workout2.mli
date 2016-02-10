module Capability : sig
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
  type absolute = private int
  type percent = private int
  type zone = private int

  val absolute_of_int : int -> absolute
  val percent_of_int : int -> percent
  val zone_of_int : int -> zone

  type t = Absolute of absolute (* bpm *)
         | Percent of percent   (* 0-100 % of max *)
end

module Power : sig
  type absolute = private int
  type percent = private int
  type zone = private int

  val absolute_of_int : int -> absolute
  val percent_of_int : int -> percent
  val zone_of_int : int -> zone

  type t = Absolute of absolute (* W *)
         | Percent of percent   (* 0-1000 % of FTP *)
end

module Condition : sig
  type order = Less | Greater

  type calories = private int   (* kcal *)
  type distance = private int   (* cm *)
  type time = private int       (* ms *)

  val calories_of_int : int -> calories
  val distance_of_int : int -> distance
  val time_of_int : int -> time

  type t = Time of time
         | Distance of distance
         | Heart_rate of order * Heart_rate.t
         | Calories of calories
         | Power of order * Power.t
end

module Repeat : sig
  type times = private int

  val times_of_int : int -> times

  type t = Times of times
         | Until of Condition.t
end

module Target : sig
  module Value : sig
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

module Intensity : sig
  type t = Active
         | Cool_down
         | Rest
         | Warm_up
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
