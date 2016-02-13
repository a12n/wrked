module Sport : sig
  type t = Cycling
         | Running
         | Swimming
         | Walking

  val of_string : string -> t

  val to_string : t -> string
end

module Speed : sig
  type t = private float        (* m/s *)
  type zone = private int

  val of_float : float -> t
  val zone_of_int : int -> zone

  val from_kmph : float -> t
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
    type range = private (S.t * S.t)

    type t = Zone of S.zone
           | Range of range

    val range_of_pair : S.t * S.t -> range
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
  } and repeat = {
    condition : Repeat.t;
    steps     : t Non_empty_list.t;
  } and t = Single of single
          | Repeat of repeat
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
