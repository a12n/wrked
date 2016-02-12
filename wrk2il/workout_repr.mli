open Batteries

val from_channel : IO.input -> Workout.t

val from_lexbuf : Lexing.lexbuf -> Workout.t

val from_string : string -> Workout.t

val to_channel : 'a IO.output -> Workout.t -> unit

val to_string : Workout.t -> string

module Ir : sig
  val to_channel : 'a IO.output -> Workout.t -> unit
end
