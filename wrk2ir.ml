let () =
  Workout_repr.from_channel stdin |> Workout_repr.Ir.to_channel stdout
