module Server_log = (val Logs.src_log (Logs.Src.create "server") : Logs.LOG)

module Main (Server : Cohttp_lwt.Server) = struct
  open Wrk

  let filename {Workout.name; sport; _} =
    "workout-" ^
    (match sport with
     | Some sport -> Workout.Sport.to_string sport ^ "-"
     | None       -> "") ^
    (match name with
     | Some name -> name
     | None      -> "") ^
    ".fit"

  let content_disposition fname = "attachment; filename=" ^ fname

  let content_type = "application/vnd.ant.fit"

  let callback _id request _body =
    let status, headers, body =
      let path = Cohttp.Request.uri request |> Uri.path in
      let wrk = String.(sub path 1 (length path - 1)) |> Uri.pct_decode in
      let hdrs = Cohttp.Header.init () in
      match Wrk.Repr.from_string wrk with
      | _workout                   -> `OK, hdrs, ""
      | exception Wrk.Lexer.Error  -> `Bad_request, hdrs, ""
      | exception Wrk.Parser.Error -> `Bad_request, hdrs, "" in
    Server.respond_string ~headers ~status ~body ()

  let conn_closed _id =
    (* TODO *)
    ()

  let start srv = srv (`TCP 8080) (Server.make ~callback ~conn_closed ())
end
