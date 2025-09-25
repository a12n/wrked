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
  let of_float_kmph = ( *. ) (1000.0 /. 3600.0)
  let to_float_kmph = ( *. ) (3600.0 /. 1000.0)
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

  let relation_to_char = function Less -> '<' | Greater -> '>'
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
  module Make (Value : sig
    type t
    type zone
  end) =
  struct
    type range = Value.t * Value.t
    type t = Zone of Value.zone | Range of range

    let range_of_pair (a, b) = if a < b then (a, b) else (b, a)
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
      let print out = function
        | Sport.Cycling c ->
            Print.string out "cycling";
            Option.iter
              (fun c ->
                Print.char out '/';
                Print.string out (Sport.cycling_to_string c))
              c
        | Running r ->
            Print.string out "running";
            Option.iter
              (fun r ->
                Print.char out '/';
                Print.string out (Sport.running_to_string r))
              r
        | Swimming s ->
            Print.string out "swimming";
            Option.iter
              (fun s ->
                Print.char out '/';
                Print.string out (Sport.swimming_to_string s))
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
      let print out = function
        | Condition.Time t ->
            Print.string out "time";
            Time_printer.print out t
        | Distance dist ->
            Print.string out "distance";
            Distance_printer.print out dist
        | Heart_rate (rel, hr) ->
            Print.string out "hr";
            Print.char out (Condition.relation_to_char rel);
            Heart_rate_printer.print out hr
        | Power (rel, pwr) ->
            Print.string out "power";
            Print.char out (Condition.relation_to_char rel);
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
      module Make (Value : sig
        type t
        type zone
      end) (Value_target : sig
        type t = Zone of Value.zone | Range of (Value.t * Value.t)
      end) (Value_printer : sig
        val print : Print.t -> Value.t -> unit
        val print_zone : Print.t -> Value.zone -> unit
      end) =
      struct
        let print out = function
          | Value_target.Zone z -> Value_printer.print_zone out z
          | Range (lo, hi) ->
              Value_printer.print out lo;
              Print.char out '-';
              Value_printer.print out hi
      end

      module Cadence_target_printer =
        Make (Cadence) (Target.Cadence_target) (Cadence_printer)

      module Heart_rate_target_printer =
        Make (Heart_rate) (Target.Heart_rate_target) (Heart_rate_printer)

      module Power_target_printer =
        Make (Power) (Target.Power_target) (Power_printer)

      module Speed_target_printer =
        Make (Speed) (Target.Speed_target) (Speed_printer)

      let print out = function
        | Target.Speed spd ->
            Print.string out "speed";
            Speed_target_printer.print out spd
        | Heart_rate hr ->
            Print.string out "hr";
            Heart_rate_target_printer.print out hr
        | Cadence cad ->
            Print.string out "cadence";
            Cadence_target_printer.print out cad
        | Power pwr ->
            Print.string out "power";
            Power_target_printer.print out pwr
    end

    module Step_printer = struct
      let print_single out Step.{ name; duration; target; intensity; _ } =
        Option.iter (print_name out) name;
        Option.iter
          (fun intensity ->
            Print.string out (Step.intensity_to_string intensity);
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

  module Buffer = Make (struct
    type t = Buffer.t

    let char = Buffer.add_char
    let float b = Printf.bprintf b "%f"
    let int b = Printf.bprintf b "%d"
    let string = Buffer.add_string
  end)

  module Channel = Make (struct
    type t = out_channel

    let char = output_char
    let float ch = Printf.fprintf ch "%f"
    let int ch = Printf.fprintf ch "%d"
    let string = output_string
  end)
end

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_space = function
    | ' ' | '\x0c' | '\n' | '\r' | '\t' | '\x0b' -> true
    | _ -> false

  let comment = char '{' *> take_while (( <> ) '}') <* char '}'
  let string = char '"' *> take_while (( <> ) '"') <* char '"'
  let int = take_while1 is_digit >>| int_of_string

  let float =
    lift2
      (fun a b -> float_of_string (a ^ "." ^ b))
      (take_while1 is_digit)
      (char '.' *> take_while1 is_digit)

  let number = float <|> (int >>| float_of_int)

  let lwsp =
    skip_while is_space *> option () (comment >>| ignore) <* skip_while is_space

  let non_empty_list elt =
    lwsp *> char '[' *> sep_by1 (lwsp *> char ';') elt <* lwsp <* char ']'
    >>= function
    | [] -> fail "empty list"
    | elt0 :: elts -> return (elt0, elts)

  module Sport_parser = struct
    let cycling =
      lwsp
      *> Sport.(
           string_ci "spin" *> return Spin
           <|> string_ci "indoor" *> return (Indoor : cycling)
           <|> string_ci "road" *> return Road
           <|> string_ci "mountain" *> return Mountain
           <|> string_ci "downhill" *> return Downhill
           <|> string_ci "recumbent" *> return Recumbent
           <|> string_ci "cyclocross" *> return Cyclocross
           <|> string_ci "hand" *> return Hand
           <|> string_ci "track" *> return (Track : cycling)
           <|> string_ci "bmx" *> return BMX
           <|> string_ci "gravel" *> return Gravel
           <|> string_ci "commuting" *> return Commuting
           <|> string_ci "mixed_surface" *> return Mixed_surface)

    let running =
      lwsp
      *> Sport.(
           string_ci "treadmill" *> return Treadmill
           <|> string_ci "street" *> return Street
           <|> string_ci "trail" *> return Trail
           <|> string_ci "track" *> return (Track : running)
           <|> string_ci "indoor" *> return (Indoor : running))

    let swimming =
      lwsp
      *> Sport.(
           string_ci "lap" *> return Lap
           <|> string_ci "open_water" *> return Open_water)

    let parser =
      lwsp
      *> (string_ci "cycling"
          *> option None (lwsp *> char '/' *> cycling >>| Option.some)
         >>| (fun c -> Sport.Cycling c)
         <|> ( string_ci "running"
             *> option None (lwsp *> char '/' *> running >>| Option.some)
             >>| fun r -> Sport.Running r )
         <|> ( string_ci "swimming"
             *> option None (lwsp *> char '/' *> swimming >>| Option.some)
             >>| fun s -> Sport.Swimming s ))
  end

  module Speed_parser = struct
    let zone = lwsp *> int >>| Speed.zone_of_int

    let parser =
      let kmph = lwsp *> option "km/h" (string_ci "km/h") in
      let mps = lwsp *> string_ci "m/s" in
      lwsp *> (number <* mps <|> (number <* kmph >>| Speed.of_float_kmph))
      >>| Speed.of_float
  end

  module Cadence_parser = struct
    let zone = lwsp *> int >>| Cadence.zone_of_int

    let parser =
      let rpm = lwsp *> option "rpm" (string_ci "rpm") in
      lwsp *> int <* rpm >>| Cadence.of_int
  end

  module Heart_rate_parser = struct
    let zone = lwsp *> int >>| Heart_rate.zone_of_int

    let parser =
      let percent = lwsp *> char '%' in
      let bpm = lwsp *> option "bpm" (string_ci "bpm") in
      lwsp
      *> (int <* percent >>| Heart_rate.of_int_relative
         <|> (int <* bpm >>| Heart_rate.of_int))
  end

  module Power_parser = struct
    let zone = lwsp *> int >>| Power.zone_of_int

    let parser =
      let percent = lwsp *> char '%' in
      let w = lwsp *> option "W" (string_ci "W") in
      lwsp
      *> (int <* percent >>| Power.of_int_relative
         <|> (int <* w >>| Power.of_int))
  end

  module Time_parser = struct
    let parser =
      let h = lwsp *> string_ci "h" in
      let min = lwsp *> string_ci "min" in
      let s = lwsp *> option "s" (string_ci "s") in
      lift3
        (fun h min s ->
          let s_h = 3600.0 *. h |> Float.round |> Float.to_int in
          let s_min = 60.0 *. min |> Float.round |> Float.to_int in
          Time.of_int (s_h + s_min + s))
        (option 0.0 (lwsp *> number <* h))
        (option 0.0 (lwsp *> number <* min))
        (option 0 (lwsp *> int <* s))
  end

  module Distance_parser = struct
    let parser =
      let km = lwsp *> string_ci "km" in
      let m = lwsp *> option "m" (string_ci "m") in
      lwsp *> (int <* km >>| ( * ) 1000 <|> (int <* m)) >>| Distance.of_int
  end

  module Calories_parser = struct
    let parser =
      let kcal = lwsp *> option "kcal" (string_ci "kcal") in
      lwsp *> int <* kcal >>| Calories.of_int
  end

  module Condition_parser = struct
    let relation =
      lwsp
      *> (char '<' *> return Condition.Less
         <|> char '>' *> return Condition.Greater)

    let parser =
      lwsp
      *> (string_ci "time" *> lwsp *> Time_parser.parser
         >>| (fun t -> Condition.Time t)
         <|> ( string_ci "distance" *> lwsp *> Distance_parser.parser
             >>| fun d -> Condition.Distance d )
         <|> ( string_ci "hr" *> lwsp *> both relation Heart_rate_parser.parser
             >>| fun h -> Condition.Heart_rate h )
         <|> ( string_ci "calories" *> lwsp *> Calories_parser.parser
             >>| fun c -> Condition.Calories c )
         <|> ( string_ci "power" *> lwsp *> both relation Power_parser.parser
             >>| fun p -> Condition.Power p ))
  end

  module Repeat_parser = struct
    let times =
      lift Repeat.times_of_int (int <* (char 'x' <|> char 'X' <|> char '*'))

    let parser =
      lwsp *> times
      >>| (fun t -> Repeat.Times t)
      <|> (Condition_parser.parser >>| fun c -> Repeat.Until c)
  end

  module Target_parser = struct
    module Make (Value : sig
      type t
      type zone
    end) (Value_target : sig
      type range = Value.t * Value.t
      type t = Zone of Value.zone | Range of range

      val range_of_pair : Value.t * Value.t -> range
    end) (Value_parser : sig
      val zone : Value.zone Angstrom.t
      val parser : Value.t Angstrom.t
    end) =
    struct
      let parser =
        let zone =
          lwsp *> string_ci "zone" *> Value_parser.zone >>| fun z ->
          Value_target.Zone z
        in
        let range =
          both Value_parser.parser (lwsp *> char '-' *> Value_parser.parser)
          >>| Value_target.range_of_pair
          >>| fun r -> Value_target.Range r
        in
        zone <|> range
    end

    module Cadence_target_parser =
      Make (Cadence) (Target.Cadence_target) (Cadence_parser)

    module Heart_rate_target_parser =
      Make (Heart_rate) (Target.Heart_rate_target) (Heart_rate_parser)

    module Power_target_parser =
      Make (Power) (Target.Power_target) (Power_parser)

    module Speed_target_parser =
      Make (Speed) (Target.Speed_target) (Speed_parser)

    let parser =
      string_ci "speed" *> Speed_target_parser.parser
      >>| (fun s -> Target.Speed s)
      <|> ( string_ci "hr" *> Heart_rate_target_parser.parser >>| fun h ->
            Target.Heart_rate h )
      <|> ( string_ci "cadence" *> Cadence_target_parser.parser >>| fun c ->
            Target.Cadence c )
      <|> ( string_ci "power" *> Power_target_parser.parser >>| fun p ->
            Target.Power p )
  end

  module Step_parser = struct
    let intensity =
      lwsp
      *> Step.(
           string_ci "active" *> return Active
           <|> string_ci "rest" *> return Rest
           <|> string_ci "warmup" *> return Warmup
           <|> string_ci "cooldown" *> return Cooldown
           <|> string_ci "recovery" *> return Recovery
           <|> string_ci "interval" *> return Interval
           <|> string_ci "other" *> return Other)

    let single =
      lift3
        (fun name intensity (duration, target) ->
          Step.{ name; descr = None; duration; target; intensity })
        (lwsp *> option None (string <* lwsp <* char ':' >>| Option.some))
        (option None (intensity <* lwsp <* char ',' >>| Option.some))
        (lwsp *> string_ci "open"
        >>| (fun _ -> (None, None))
        <|> ( both Condition_parser.parser
                (lwsp *> char ',' *> Target_parser.parser)
            >>| fun (c, t) -> (Some c, Some t) )
        <|> (Condition_parser.parser >>| fun c -> (Some c, None))
        <|> (Target_parser.parser >>| fun t -> (None, Some t)))

    let repeat step =
      lift2
        (fun repeat steps -> Step.{ repeat; steps })
        (lwsp *> char '(' *> Repeat_parser.parser <* lwsp <* char ')')
        (non_empty_list step)

    let parser =
      fix (fun step ->
          single
          >>| (fun s -> Step.Single s)
          <|> (repeat step >>| fun r -> Step.Repeat r))
  end

  let parser =
    lift3
      (fun name sport steps -> { name; descr = None; sport; steps })
      (lwsp *> option None (string <* lwsp <* char ':' >>| Option.some))
      (option None (Sport_parser.parser >>| Option.some))
      (non_empty_list Step_parser.parser <* lwsp <* end_of_input)

  let parse_channel ch =
    let len = 1024 in
    let buf = Bytes.create len in
    let rec loop state =
      match input ch buf 0 len with
      | 0 -> Buffered.feed state `Eof
      | n when n = len ->
          Buffered.feed state (`String Bytes.(unsafe_to_string buf)) |> loop
      | n ->
          Buffered.feed state (`String Bytes.(unsafe_to_string (sub buf 0 n)))
          |> loop
    in
    match loop (Buffered.parse ~initial_buffer_size:len parser) with
    | Done (_, w) -> Ok w
    | Fail (_, _, msg) -> Error msg
    | Partial _ -> Error "partial"

  let parse_string = parse_string ~consume:Consume.All parser
end
