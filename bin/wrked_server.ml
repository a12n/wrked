(* https://ninenines.eu/docs/en/cowboy/2.12/guide/rest_flowcharts/ *)

let allowed_methods req =
  (* known_methods *)
  match Http.Request.meth req with
  | `GET -> Ok ()
  | `Other _ -> Error (`Not_implemented, None)
  | _ -> Error (`Method_not_allowed, None)

let resource_exists req =
  match
    Http.Request.resource req |> String.split_on_char '/'
    |> List.filter (( <> ) "")
  with
  | [ "v1"; resource ] -> Ok (Uri.pct_decode resource)
  | _ -> Error (`Not_found, None)

let uri_too_long workout_descr =
  if String.length workout_descr > 32 * 1024 then
    Error (`Request_uri_too_long, None)
  else Ok ()

let content_types_provided req =
  if
    List.exists
      (function
        | _, (Cohttp.Accept.AnyMedia, _)
        | _, (AnyMediaSubtype "application", _)
        | _, (MediaType ("application", "vnd.ant.fit"), _) ->
            true
        | _ -> false)
      (Cohttp.Accept.media_ranges
         (Http.Header.get_multi_concat (Http.Request.headers req) "accept"))
  then Ok ()
  else Error (`Not_acceptable, None)

let malformed_request workout_descr =
  Workout.Parser.parse_string workout_descr
  |> Result.map_error (fun _ -> (`Bad_request, None))

let moved_permanently workout_descr workout =
  let min_workout_descr =
    let b = Buffer.create (String.length workout_descr) in
    Workout.Printer.Buffer.print b workout;
    Buffer.contents b
  in
  if workout_descr <> min_workout_descr then
    let location = "/v1/" ^ Uri.pct_encode ~scheme:"http" min_workout_descr in
    Error (`Moved_permanently, Some (Http.Header.init_with "location" location))
  else Ok ()

let last_modified _req =
  (* TODO *)
  Ok ()

let to_fit _workout =
  (* TODO *)
  Bytes.empty

let callback _conn req _body =
  match
    let ( let* ) = Result.bind in
    let* () = allowed_methods req in
    let* workout_descr = resource_exists req in
    let* () = uri_too_long workout_descr in
    let* () = content_types_provided req in
    let* () = last_modified req in
    let* workout = malformed_request workout_descr in
    let* () = moved_permanently workout_descr workout in
    Ok
      ( Some (Http.Header.init_with "content-type" "application/vnd.ant.fit"),
        Bytes.unsafe_to_string (to_fit workout) )
  with
  | Ok (headers, body) ->
      Cohttp_eio.Server.respond_string ~status:`OK ?headers ~body ()
  | Error (status, headers) ->
      Cohttp_eio.Server.respond_string ~status ?headers ~body:"" ()

let on_error exn = prerr_endline (Printexc.to_string exn)

let main env =
  let net = Eio.Stdenv.net env in
  Eio.Switch.run (fun sw ->
      let backlog = 128 in
      let reuse_addr = true in
      let socket =
        Eio.Net.listen ~sw ~backlog ~reuse_addr net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 8080))
      in
      let server = Cohttp_eio.Server.make ~callback () in
      Cohttp_eio.Server.run ~on_error socket server)

let () = Eio_main.run main
