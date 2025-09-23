type 'a non_empty_list = 'a * 'a list

module Sport : sig
  type cycling =
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
    | Mixed_surface

  type running = Treadmill | Street | Trail | Track | Indoor
  type swimming = Lap | Open_water

  type t =
    | Cycling of cycling option
    | Running of running option
    | Swimming of swimming option

  val of_string : string -> t
  val to_string : t -> string
end

module Speed : sig
  type t = private float (* m/s *)
  type zone = private int (* 1–10 *)

  val of_float : float -> t
  val zone_of_int : int -> zone
end

module Cadence : sig
  type t = private int (* rpm *)
  type zone = private int (* 1–? *)

  val of_int : int -> t
  val zone_of_int : int -> zone
end

module Heart_rate : sig
  type t = private
    | Absolute of int (* bpm *)
    | Relative of int (* 0-100 % of max *)

  type zone = private int (* 1-5 *)

  val of_int : int -> t
  val of_int_relative : int -> t
  val zone_of_int : int -> zone
end

module Power : sig
  type t = private
    | Absolute of int (* W *)
    | Relative of int (* 0-1000 % of FTP *)

  type zone = private int (* 1-7 *)

  val of_int : int -> t
  val of_int_relative : int -> t
  val zone_of_int : int -> zone
end

end

module Condition : sig
  type relation = Less | Greater
  type calories = private int (* kcal *)
  type distance = private int (* m *)
  type time = private int (* s *)

  val calories_of_int : int -> calories
  val distance_of_int : int -> distance
  val time_of_int : int -> time

  type t =
    | Time of time
    | Distance of distance
    | Heart_rate of (relation * Heart_rate.t)
    | Calories of calories
    | Power of (relation * Power.t)
end

module Repeat : sig
  type times = private int

  val times_of_int : int -> times

  type t = Times of times | Until of Condition.t
end

module Target : sig
  module Value : functor
    (S : sig
       type t
       type zone
     end)
    -> sig
    type range = private S.t * S.t
    type t = Zone of S.zone | Range of range

    val range_of_pair : S.t * S.t -> range
  end

  module Cadence : module type of Value (Cadence)
  module Heart_rate : module type of Value (Heart_rate)
  module Power : module type of Value (Power)
  module Speed : module type of Value (Speed)

  type t =
    | Speed of Speed.t
    | Heart_rate of Heart_rate.t
    | Cadence of Cadence.t
    | Power of Power.t
end

module Intensity : sig
  type t = Active | Rest | Warmup | Cooldown | Recovery | Interval | Other

  val to_string : t -> string
end

module Step : sig
  type single = {
    name : string option;
    descr : string option;
    duration : Condition.t option;
    target : Target.t option;
    intensity : Intensity.t option;
  }

  and repeat = { condition : Repeat.t; steps : t non_empty_list }
  and t = Single of single | Repeat of repeat
end

module Capabilities : sig
  type t = {
    speed : bool;
    heart_rate : bool;
    distance : bool;
    cadence : bool;
    power : bool;
    grade : bool;
    resistance : bool;
  }

  val zero : t
end

type t = {
  name : string option;
  descr : string option;
  sport : Sport.t option;
  steps : Step.t non_empty_list;
}

val caps : t -> Capabilities.t
