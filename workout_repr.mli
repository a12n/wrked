open Batteries

val from_channel : IO.input -> Workout.t

val from_string : string -> Workout.t

module Ir : sig
  val to_channel : 'a IO.output -> Workout.t -> unit
end
