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

module Time : sig
  type t = private int (* s *)

  val of_int : int -> t
end

module Distance : sig
  type t = private int (* m *)

  val of_int : int -> t
end

module Calories : sig
  type t = private int (* kcal *)

  val of_int : int -> t
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
end

module Repeat : sig
  type times = private int

  val times_of_int : int -> times

  type t = Times of times | Until of Condition.t

  val caps : t -> Capabilities.t
end

module Target : sig
  module Make : functor
    (Value : sig
       type t
       type zone
     end)
    -> sig
    type range = private Value.t * Value.t
    type t = Zone of Value.zone | Range of range

    val range_of_pair : Value.t * Value.t -> range
  end

  module Cadence_target : module type of Make (Cadence)
  module Heart_rate_target : module type of Make (Heart_rate)
  module Power_target : module type of Make (Power)
  module Speed_target : module type of Make (Speed)

  type t =
    | Speed of Speed_target.t
    | Heart_rate of Heart_rate_target.t
    | Cadence of Cadence_target.t
    | Power of Power_target.t

  val caps : t -> Capabilities.t
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
end

type t = {
  name : string option;
  descr : string option;
  sport : Sport.t option;
  steps : Step.t non_empty_list;
}

val caps : t -> Capabilities.t

module Printer : sig
  module Make : functor
    (Print : sig
       type t

       val char : t -> char -> unit
       val float : t -> float -> unit
       val int : t -> int -> unit
       val string : t -> string -> unit
     end)
    -> sig
    module Sport_printer : sig
      val print : Print.t -> Sport.t -> unit
    end

    module Speed_printer : sig
      val print : Print.t -> Speed.t -> unit
      val print_zone : Print.t -> Speed.zone -> unit
    end

    module Cadence_printer : sig
      val print : Print.t -> Cadence.t -> unit
      val print_zone : Print.t -> Cadence.zone -> unit
    end

    module Heart_rate_printer : sig
      val print : Print.t -> Heart_rate.t -> unit
      val print_zone : Print.t -> Heart_rate.zone -> unit
    end

    module Power_printer : sig
      val print : Print.t -> Power.t -> unit
      val print_zone : Print.t -> Power.zone -> unit
    end

    module Time_printer : sig
      val print : Print.t -> Time.t -> unit
    end

    module Distance_printer : sig
      val print : Print.t -> Distance.t -> unit
    end

    module Calories_printer : sig
      val print : Print.t -> Calories.t -> unit
    end

    module Condition_printer : sig
      val print : Print.t -> Condition.t -> unit
    end

    module Repeat_printer : sig
      val print : Print.t -> Repeat.t -> unit
    end

    module Target_printer : sig
      (* TODO: Export separate Value_target_printer modules? *)
      val print : Print.t -> Target.t -> unit
    end

    module Step_printer : sig
      val print_single : Print.t -> Step.single -> unit
      val print_repeat : Print.t -> Step.repeat -> unit
      val print : Print.t -> Step.t -> unit
    end

    val print : Print.t -> t -> unit
  end

  module Channel : module type of Make (struct
    type t = out_channel

    let char = output_char
    let float ch = Printf.fprintf ch "%f"
    let int ch = Printf.fprintf ch "%d"
    let string = output_string
  end)
end
