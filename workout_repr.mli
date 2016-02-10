val from_channel : in_channel -> Workout.t

module Ir : sig
  val to_channel : out_channel -> Workout.t -> unit
end
