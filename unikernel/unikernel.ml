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
     | None ->
       let {Unix.tm_year; tm_mon; tm_mday;
            tm_hour; tm_min; tm_sec; _} = Unix.(gmtime (time ())) in
       Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ"
         (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec) ^
    ".fit"

  let content_disposition fname = "attachment; filename=" ^ fname

  let content_type = "application/vnd.ant.fit"

  let callback _id request _body =
    let status, headers, body =
      try
        let path = Cohttp.Request.uri request |> Uri.path in
        let wrk = String.(sub path 1 (length path - 1)) |> Uri.pct_decode in
        let workout = Repr.from_string wrk in
        `OK,
        Some (Cohttp.Header.of_list
                [ "content-disposition", content_disposition (filename workout);
                  "content-type", content_type ]),
        Repr.to_string workout
      with Lexer.Error | Parser.Error ->
        `Bad_request, None, "" in
    Server.respond_string ?headers ~status ~body ()

  let conn_closed _id =
    (* TODO *)
    ()

  let start srv = srv (`TCP 8080) (Server.make ~callback ~conn_closed ())
end
