type 'a non_empty_list = 'a * 'a list

module Capabilities = struct
  type t = {
    speed : bool;
    heart_rate : bool;
    distance : bool;
    cadence : bool;
    power : bool;
    grade : bool;
    resistance : bool;
  }

  let zero =
    {
      speed = false;
      heart_rate = false;
      distance = false;
      cadence = false;
      power = false;
      grade = false;
      resistance = false;
    }

  let logor n m =
    {
      speed = n.speed || m.speed;
      heart_rate = n.heart_rate || m.heart_rate;
      distance = n.distance || m.distance;
      cadence = n.cadence || m.cadence;
      power = n.power || m.power;
      grade = n.grade || m.grade;
      resistance = n.resistance || m.resistance;
    }
end

module Sport = struct
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

  let cycling_to_string = function
    | Spin -> "spin"
    | Indoor -> "indoor"
    | Road -> "road"
    | Mountain -> "mountain"
    | Downhill -> "downhill"
    | Recumbent -> "recumbent"
    | Cyclocross -> "cyclocross"
    | Hand -> "hand"
    | Track -> "track"
    | BMX -> "bmx"
    | Gravel -> "gravel"
    | Commuting -> "commuting"
    | Mixed_surface -> "mixed_surface"

  let running_to_string = function
    | Treadmill -> "treadmill"
    | Street -> "street"
    | Trail -> "trail"
    | Track -> "track"
    | Indoor -> "indoor"

  let swimming_to_string = function Lap -> "lap" | Open_water -> "open_water"
end

let restricted (min, max) exn = function
  | n when n >= min && n <= max -> n
  | _ -> raise exn

module Speed = struct
  type t = float
  type zone = int

  let of_float = restricted (1.0, 100.0) (Invalid_argument __FUNCTION__)
  let zone_of_int = restricted (1, 10) (Invalid_argument __FUNCTION__)
end

module Cadence = struct
  type t = int
  type zone = int

  let of_int = restricted (1, 500) (Invalid_argument __FUNCTION__)

  (* TODO: Max cadence zone? *)
  let zone_of_int = restricted (1, 10) (Invalid_argument __FUNCTION__)
end

module Heart_rate = struct
  type t = Absolute of int | Relative of int
  type zone = int

  let of_int bpm =
    Absolute (restricted (1, 255) (Invalid_argument __FUNCTION__) bpm)

  let of_int_relative pct =
    Relative (restricted (1, 100) (Invalid_argument __FUNCTION__) pct)

  let zone_of_int = restricted (1, 5) (Invalid_argument __FUNCTION__)
end

module Power = struct
  type t = Absolute of int | Relative of int
  type zone = int

  let of_int w =
    Absolute (restricted (1, 10000) (Invalid_argument __FUNCTION__) w)

  let of_int_relative pct =
    Relative (restricted (1, 1000) (Invalid_argument __FUNCTION__) pct)

  let zone_of_int = restricted (1, 7) (Invalid_argument __FUNCTION__)
end

module Time = struct
  type t = int

  let of_int = restricted (1, max_int) (Invalid_argument __FUNCTION__)
end

module Distance = struct
  type t = int

  let of_int = restricted (1, max_int) (Invalid_argument __FUNCTION__)
end

module Calories = struct
  type t = int

  let of_int = restricted (1, max_int) (Invalid_argument __FUNCTION__)
end

module Condition = struct
  type relation = Less | Greater

  type t =
    | Time of Time.t
    | Distance of Distance.t
    | Heart_rate of (relation * Heart_rate.t)
    | Calories of Calories.t
    | Power of (relation * Power.t)

  let caps = function
    | Distance _ -> Capabilities.{ zero with distance = true }
    | Heart_rate _ -> Capabilities.{ zero with heart_rate = true }
    | Power _ -> Capabilities.{ zero with power = true }
    | Calories _ | Time _ -> Capabilities.zero
end

module Repeat = struct
  type times = int

  let times_of_int = restricted (2, 1000000) (Invalid_argument __FUNCTION__)

  type t = Times of times | Until of Condition.t

  let caps = function
    | Times _ -> Capabilities.zero
    | Until c -> Condition.caps c
end

module Target = struct
  module Value (S : sig
    type t
    type zone
  end) =
  struct
    type range = S.t * S.t
    type t = Zone of S.zone | Range of range

    let range_of_pair (a, b) = if a < b then (a, b) else (b, a)
  end

  module Cadence = Value (Cadence)
  module Heart_rate = Value (Heart_rate)
  module Power = Value (Power)
  module Speed = Value (Speed)

  type t =
    | Speed of Speed.t
    | Heart_rate of Heart_rate.t
    | Cadence of Cadence.t
    | Power of Power.t

  let caps = function
    | Speed _ -> Capabilities.{ zero with speed = true }
    | Heart_rate _ -> Capabilities.{ zero with heart_rate = true }
    | Cadence _ -> Capabilities.{ zero with cadence = true }
    | Power _ -> Capabilities.{ zero with power = true }
end

module Step = struct
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

  let rec caps = function
    | Single { duration; target; _ } ->
        Capabilities.logor
          (match duration with
          | Some duration -> Condition.caps duration
          | None -> Capabilities.zero)
          (match target with
          | Some target -> Target.caps target
          | None -> Capabilities.zero)
    | Repeat { repeat; steps = step0, steps } ->
        Capabilities.logor (Repeat.caps repeat)
          (List.fold_left Capabilities.logor (caps step0) (List.map caps steps))

  let intensity_to_string = function
    | Active -> "active"
    | Rest -> "rest"
    | Warmup -> "warmup"
    | Cooldown -> "cooldown"
    | Recovery -> "recovery"
    | Interval -> "interval"
    | Other -> "other"
end

type t = {
  name : string option;
  descr : string option;
  sport : Sport.t option;
  steps : Step.t non_empty_list;
}

let caps { steps = step0, steps; _ } =
  List.fold_left Capabilities.logor (Step.caps step0) (List.map Step.caps steps)
