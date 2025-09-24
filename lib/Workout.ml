type 'a non_empty_list = 'a * 'a list

let name_to_buffer b name =
  Buffer.(
    add_char b '"';
    String.iter
      (function
        | '"' ->
            (* TODO: Escape? *)
            add_char b ' '
        | c -> add_char b c)
      name;
    add_char b '"';
    add_char b ':')

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

  let to_buffer b = function
    | Cycling c ->
        Buffer.add_string b "cycling";
        Option.iter
          (fun c ->
            Buffer.(
              add_char b '/';
              add_string b (cycling_to_string c)))
          c
    | Running r ->
        Buffer.add_string b "running";
        Option.iter
          (fun r ->
            Buffer.(
              add_char b '/';
              add_string b (running_to_string r)))
          r
    | Swimming s ->
        Buffer.add_string b "swimming";
        Option.iter
          (fun s ->
            Buffer.(
              add_char b '/';
              add_string b (swimming_to_string s)))
          s
end

let restricted (min, max) exn = function
  | n when n >= min && n <= max -> n
  | _ -> raise exn

module Speed = struct
  type t = float
  type zone = int

  let of_float = restricted (1.0, 100.0) (Invalid_argument __FUNCTION__)
  let zone_of_int = restricted (1, 10) (Invalid_argument __FUNCTION__)
  let to_float_kmph = ( *. ) (3600.0 /. 1000.0)

  (* While speed is in m/s internally, it's in km/h in the text format *)
  let to_buffer b mps = Printf.bprintf b "%f" (to_float_kmph mps)
  let zone_to_buffer b z = Printf.bprintf b "%d" z
end

module Cadence = struct
  type t = int
  type zone = int

  let of_int = restricted (1, 500) (Invalid_argument __FUNCTION__)

  (* TODO: Max cadence zone? *)
  let to_buffer b rpm = Printf.bprintf b "%d" rpm
  let zone_of_int = restricted (1, 10) (Invalid_argument __FUNCTION__)
  let zone_to_buffer b z = Printf.bprintf b "%d" z
end

module Heart_rate = struct
  type t = Absolute of int | Relative of int
  type zone = int

  let of_int bpm =
    Absolute (restricted (1, 255) (Invalid_argument __FUNCTION__) bpm)

  let of_int_relative pct =
    Relative (restricted (1, 100) (Invalid_argument __FUNCTION__) pct)

  let zone_of_int = restricted (1, 5) (Invalid_argument __FUNCTION__)

  let to_buffer b = function
    | Absolute bpm -> Printf.bprintf b "%d" bpm
    | Relative percent -> Printf.bprintf b "%d%%" percent

  let zone_to_buffer b z = Printf.bprintf b "%d" z
end

module Power = struct
  type t = Absolute of int | Relative of int
  type zone = int

  let of_int w =
    Absolute (restricted (1, 10000) (Invalid_argument __FUNCTION__) w)

  let of_int_relative pct =
    Relative (restricted (1, 1000) (Invalid_argument __FUNCTION__) pct)

  let zone_of_int = restricted (1, 7) (Invalid_argument __FUNCTION__)

  let to_buffer b = function
    | Absolute w -> Printf.bprintf b "%d" w
    | Relative percent -> Printf.bprintf b "%d%%" percent

  let zone_to_buffer b z = Printf.bprintf b "%d" z
end

module Time = struct
  type t = int

  let of_int = restricted (1, max_int) (Invalid_argument __FUNCTION__)

  let to_buffer b s =
    let min, s = (s / 60, s mod 60) in
    let h, min = (min / 60, min mod 60) in
    if h <> 0 then Printf.bprintf b "%dh" h;
    if min <> 0 then Printf.bprintf b "%dmin" min;
    if s <> 0 then Printf.bprintf b "%ds" s
end

module Distance = struct
  type t = int

  let of_int = restricted (1, max_int) (Invalid_argument __FUNCTION__)
  let to_buffer b m = Printf.bprintf b "%d" m
end

module Calories = struct
  type t = int

  let of_int = restricted (1, max_int) (Invalid_argument __FUNCTION__)
  let to_buffer b kcal = Printf.bprintf b "%d" kcal
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

  let relation_to_char = function Less -> '<' | Greater -> '>'

  let to_buffer b = function
    | Time t ->
        Buffer.add_string b "time";
        Time.to_buffer b t
    | Distance d ->
        Buffer.add_string b "distance";
        Distance.to_buffer b d
    | Heart_rate (r, h) ->
        Buffer.(
          add_string b "hr";
          add_char b (relation_to_char r));
        Heart_rate.to_buffer b h
    | Power (r, p) ->
        Buffer.(
          add_string b "power";
          add_char b (relation_to_char r));
        Power.to_buffer b p
    | Calories c ->
        Buffer.add_string b "calories";
        Calories.to_buffer b c
end

module Repeat = struct
  type times = int

  let times_of_int = restricted (2, 1000000) (Invalid_argument __FUNCTION__)

  type t = Times of times | Until of Condition.t

  let caps = function
    | Times _ -> Capabilities.zero
    | Until c -> Condition.caps c

  let to_buffer b = function
    | Times n -> Printf.bprintf b "%dx" n
    | Until c -> Condition.to_buffer b c
end

module Target = struct
  module Make (S : sig
    type t
    type zone

    val to_buffer : Buffer.t -> t -> unit
    val zone_to_buffer : Buffer.t -> zone -> unit
  end) =
  struct
    type range = S.t * S.t
    type t = Zone of S.zone | Range of range

    let range_of_pair (a, b) = if a < b then (a, b) else (b, a)

    let to_buffer b = function
      | Zone z ->
          Buffer.add_string b "zone";
          S.zone_to_buffer b z
      | Range (l, h) ->
          S.to_buffer b l;
          Buffer.add_char b '-';
          S.to_buffer b h
  end

  module Cadence_target = Make (Cadence)
  module Heart_rate_target = Make (Heart_rate)
  module Power_target = Make (Power)
  module Speed_target = Make (Speed)

  type t =
    | Speed of Speed_target.t
    | Heart_rate of Heart_rate_target.t
    | Cadence of Cadence_target.t
    | Power of Power_target.t

  let caps = function
    | Speed _ -> Capabilities.{ zero with speed = true }
    | Heart_rate _ -> Capabilities.{ zero with heart_rate = true }
    | Cadence _ -> Capabilities.{ zero with cadence = true }
    | Power _ -> Capabilities.{ zero with power = true }

  let to_buffer b = function
    | Speed s ->
        Buffer.add_string b "speed";
        Speed_target.to_buffer b s
    | Heart_rate h ->
        Buffer.add_string b "hr";
        Heart_rate_target.to_buffer b h
    | Cadence c ->
        Buffer.add_string b "cadence";
        Cadence_target.to_buffer b c
    | Power p ->
        Buffer.add_string b "power";
        Power_target.to_buffer b p
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

  let single_to_buffer b { name; duration; target; intensity; _ } =
    Option.iter (name_to_buffer b) name;
    Option.iter
      (fun intensity ->
        Buffer.(
          add_string b (intensity_to_string intensity);
          add_char b ','))
      intensity;
    match (duration, target) with
    | None, None -> Buffer.add_string b "open"
    | Some c, None -> Condition.to_buffer b c
    | None, Some t -> Target.to_buffer b t
    | Some c, Some t ->
        Condition.to_buffer b c;
        Buffer.add_char b ',';
        Target.to_buffer b t

  let rec repeat_to_buffer b { repeat; steps = step0, steps } =
    Buffer.add_char b '(';
    Repeat.to_buffer b repeat;
    Buffer.add_char b ')';
    Buffer.add_char b '[';
    to_buffer b step0;
    List.iter
      (fun step ->
        Buffer.add_char b ';';
        to_buffer b step)
      steps;
    Buffer.add_char b ']'

  and to_buffer b = function
    | Single s -> single_to_buffer b s
    | Repeat r -> repeat_to_buffer b r
end

type t = {
  name : string option;
  descr : string option;
  sport : Sport.t option;
  steps : Step.t non_empty_list;
}

let caps { steps = step0, steps; _ } =
  List.fold_left Capabilities.logor (Step.caps step0) (List.map Step.caps steps)

let to_buffer b { name; sport; steps = step0, steps; _ } =
  Option.iter (name_to_buffer b) name;
  Option.iter (Sport.to_buffer b) sport;
  Buffer.add_char b '[';
  Step.to_buffer b step0;
  List.iter
    (fun step ->
      Buffer.add_char b ';';
      Step.to_buffer b step)
    steps;
  Buffer.add_char b ']'

module Printer = struct
  module Make (Print : sig
    type t

    val char : t -> char -> unit
    val float : t -> float -> unit
    val int : t -> int -> unit
    val string : t -> string -> unit
  end) =
  struct
    let print_name out name =
      Print.char out '"';
      String.iter
        (function
          | '"' ->
              (* TODO: Escape? *)
              Print.char out ' '
          | c -> Print.char out c)
        name;
      Print.char out '"';
      Print.char out ':'

    module Sport_printer = struct
      let cycling_to_string = function
        | Sport.Spin -> "spin"
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
        | Sport.Treadmill -> "treadmill"
        | Street -> "street"
        | Trail -> "trail"
        | Track -> "track"
        | Indoor -> "indoor"

      let swimming_to_string = function
        | Sport.Lap -> "lap"
        | Open_water -> "open_water"

      let print out = function
        | Sport.Cycling c ->
            Print.string out "cycling";
            Option.iter
              (fun c ->
                Print.char out '/';
                Print.string out (cycling_to_string c))
              c
        | Running r ->
            Print.string out "running";
            Option.iter
              (fun r ->
                Print.char out '/';
                Print.string out (running_to_string r))
              r
        | Swimming s ->
            Print.string out "swimming";
            Option.iter
              (fun s ->
                Print.char out '/';
                Print.string out (swimming_to_string s))
              s
    end

    module Speed_printer = struct
      (* While speed is in m/s internally, it's in km/h in the text format *)
      let print out mps = Print.float out (Speed.to_float_kmph mps)
      let print_zone = Print.int
    end

    module Cadence_printer = struct
      let print = Print.int
      let print_zone = Print.int
    end

    module Heart_rate_printer = struct
      let print out = function
        | Heart_rate.Absolute bpm -> Print.int out bpm
        | Relative percent ->
            Print.(
              int out percent;
              char out '%')

      let print_zone = Print.int
    end

    module Power_printer = struct
      let print out = function
        | Power.Absolute w -> Print.int out w
        | Relative percent ->
            Print.(
              int out percent;
              char out '%')

      let print_zone = Print.int
    end

    module Time_printer = struct
      let print out s =
        let min, s = (s / 60, s mod 60) in
        let h, min = (min / 60, min mod 60) in
        if h <> 0 then (
          Print.(
            int out h;
            char out 'h'));
        if min <> 0 then (
          Print.(
            int out min;
            string out "min"));
        if s <> 0 then (
          Print.(
            int out s;
            char out 's'))
    end

    module Distance_printer = struct
      let print = Print.int
    end

    module Calories_printer = struct
      let print = Print.int
    end

    module Condition_printer = struct
      let relation_to_char = function Condition.Less -> '<' | Greater -> '>'

      let print out = function
        | Condition.Time t ->
            Print.string out "time";
            Time_printer.print out t
        | Distance dist ->
            Print.string out "distance";
            Distance_printer.print out dist
        | Heart_rate (rel, hr) ->
            Print.string out "hr";
            Print.char out (relation_to_char rel);
            Heart_rate_printer.print out hr
        | Power (rel, pwr) ->
            Print.string out "power";
            Print.char out (relation_to_char rel);
            Power_printer.print out pwr
        | Calories cal ->
            Print.string out "calories";
            Calories_printer.print out cal
    end

    module Repeat_printer = struct
      let print out = function
        | Repeat.Times n ->
            Print.int out n;
            Print.char out 'x'
        | Until cond -> Condition_printer.print out cond
    end

    module Target_printer = struct
      module Make (S : sig
        type t
        type zone
      end) (Target : sig
        type t = Zone of S.zone | Range of (S.t * S.t)
      end) (Printer : sig
        val print : Print.t -> S.t -> unit
        val print_zone : Print.t -> S.zone -> unit
      end) =
      struct
        let print out = function
          | Target.Zone z -> Printer.print_zone out z
          | Target.Range (lo, hi) ->
              Printer.print out lo;
              Print.char out '-';
              Printer.print out hi
      end

      module Speed = Make (Speed) (Target.Speed_target) (Speed_printer)

      module Heart_rate =
        Make (Heart_rate) (Target.Heart_rate_target) (Heart_rate_printer)

      module Cadence = Make (Cadence) (Target.Cadence_target) (Cadence_printer)
      module Power = Make (Power) (Target.Power_target) (Power_printer)

      let print out = function
        | Target.Speed spd ->
            Print.string out "speed";
            Speed.print out spd
        | Heart_rate hr ->
            Print.string out "hr";
            Heart_rate.print out hr
        | Cadence cad ->
            Print.string out "cadence";
            Cadence.print out cad
        | Power pwr ->
            Print.string out "power";
            Power.print out pwr
    end

    module Step_printer = struct
      let intensity_to_string = function
        | Step.Active -> "active"
        | Rest -> "rest"
        | Warmup -> "warmup"
        | Cooldown -> "cooldown"
        | Recovery -> "recovery"
        | Interval -> "interval"
        | Other -> "other"

      let print_single out Step.{ name; duration; target; intensity; _ } =
        Option.iter (print_name out) name;
        Option.iter
          (fun intensity ->
            Print.string out (intensity_to_string intensity);
            Print.char out ',')
          intensity;
        match (duration, target) with
        | None, None -> Print.string out "open"
        | Some c, None -> Condition_printer.print out c
        | None, Some t -> Target_printer.print out t
        | Some c, Some t ->
            Condition_printer.print out c;
            Print.char out ',';
            Target_printer.print out t

      let rec print_repeat out Step.{ repeat; steps = step0, steps } =
        Print.char out '(';
        Repeat_printer.print out repeat;
        Print.char out ')';
        Print.char out '[';
        print out step0;
        List.iter
          (fun step ->
            Print.char out ';';
            print out step)
          steps;
        Print.char out ']'

      and print out = function
        | Step.Single s -> print_single out s
        | Repeat r -> print_repeat out r
    end

    let print out { name; sport; steps = step0, steps; _ } =
      Option.iter (print_name out) name;
      Option.iter (Sport_printer.print out) sport;
      Print.char out '[';
      Step_printer.print out step0;
      List.iter
        (fun step ->
          Print.char out ';';
          Step_printer.print out step)
        steps;
      Print.char out ']'
  end

  module Channel = Make (struct
    type t = out_channel

    let char = output_char
    let float ch = Printf.fprintf ch "%f"
    let int ch = Printf.fprintf ch "%d"
    let string = output_string
  end)
end
