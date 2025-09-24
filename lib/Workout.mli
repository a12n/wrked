type 'a non_empty_list = 'a * 'a list

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
end

module Speed : sig
  type t = private float (* m/s *)
  type zone = private int (* 1–10 *)

  val of_float : float -> t
  val to_buffer : Buffer.t -> t -> unit
  val zone_of_int : int -> zone
  val zone_to_buffer : Buffer.t -> zone -> unit
end

module Cadence : sig
  type t = private int (* rpm *)
  type zone = private int (* 1–? *)

  val of_int : int -> t
  val to_buffer : Buffer.t -> t -> unit
  val zone_of_int : int -> zone
  val zone_to_buffer : Buffer.t -> zone -> unit
end

module Heart_rate : sig
  type t = private
    | Absolute of int (* bpm *)
    | Relative of int (* 0-100 % of max *)

  type zone = private int (* 1-5 *)

  val of_int : int -> t
  val of_int_relative : int -> t
  val to_buffer : Buffer.t -> t -> unit
  val zone_of_int : int -> zone
  val zone_to_buffer : Buffer.t -> zone -> unit
end

module Power : sig
  type t = private
    | Absolute of int (* W *)
    | Relative of int (* 0-1000 % of FTP *)

  type zone = private int (* 1-7 *)

  val of_int : int -> t
  val of_int_relative : int -> t
  val to_buffer : Buffer.t -> t -> unit
  val zone_of_int : int -> zone
  val zone_to_buffer : Buffer.t -> zone -> unit
end

module Time : sig
  type t = private int (* s *)

  val of_int : int -> t
  val to_buffer : Buffer.t -> t -> unit
end

module Distance : sig
  type t = private int (* m *)

  val of_int : int -> t
  val to_buffer : Buffer.t -> t -> unit
end

module Calories : sig
  type t = private int (* kcal *)

  val of_int : int -> t
  val to_buffer : Buffer.t -> t -> unit
end

module Condition : sig
  type relation = Less | Greater

  type t =
    | Time of Time.t
    | Distance of Distance.t
    | Heart_rate of (relation * Heart_rate.t)
    | Calories of Calories.t
    | Power of (relation * Power.t)

  val caps : t -> Capabilities.t
  val to_buffer : Buffer.t -> t -> unit
end

module Repeat : sig
  type times = private int

  val times_of_int : int -> times

  type t = Times of times | Until of Condition.t

  val caps : t -> Capabilities.t
  val to_buffer : Buffer.t -> t -> unit
end

module Target : sig
  module Make : functor
    (S : sig
       type t
       type zone

       val to_buffer : Buffer.t -> t -> unit
       val zone_to_buffer : Buffer.t -> zone -> unit
     end)
    -> sig
    type range = private S.t * S.t
    type t = Zone of S.zone | Range of range

    val range_of_pair : S.t * S.t -> range
    val to_buffer : Buffer.t -> t -> unit
  end

  module Cadence : module type of Make (Cadence)
  module Heart_rate : module type of Make (Heart_rate)
  module Power : module type of Make (Power)
  module Speed : module type of Make (Speed)

  type t =
    | Speed of Speed.t
    | Heart_rate of Heart_rate.t
    | Cadence of Cadence.t
    | Power of Power.t

  val caps : t -> Capabilities.t
  val to_buffer : Buffer.t -> t -> unit
end

module Step : sig
  type intensity =
    | Active
    | Rest
    | Warmup
    | Cooldown
    | Recovery
    | Interval
    | Other

  type single = {
    name : string option;
    descr : string option;
    duration : Condition.t option;
    target : Target.t option;
    intensity : intensity option;
  }

  and repeat = { repeat : Repeat.t; steps : t non_empty_list }
  and t = Single of single | Repeat of repeat

  val caps : t -> Capabilities.t
  val to_buffer : Buffer.t -> t -> unit
end

type t = {
  name : string option;
  descr : string option;
  sport : Sport.t option;
  steps : Step.t non_empty_list;
}

val caps : t -> Capabilities.t
val to_buffer : Buffer.t -> t -> unit
