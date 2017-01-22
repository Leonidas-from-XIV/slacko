(* A (currently pretty useless) fake slack implementation to run tests
   against. *)
open Lwt
open Cohttp
open Cohttp_lwt_unix


let channels_json = Yojson.Safe.from_file "channels.json"


let ok_resp key value =
  `Assoc ["ok", `Bool true; key, value]

let server ?(port=7357) ~stop () =
  let callback _conn req body =
    (* let uri = req |> Request.uri |> Uri.to_string in *)
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    body |> Cohttp_lwt_body.to_string >|= (fun body ->
    ok_resp "channels" channels_json |> Yojson.Safe.to_string)
    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port port)) ~stop (Server.make ~callback ())

let with_fake_slack f =
  let stop, wake = wait () in
  let srv = server ~stop () in
  let stop_server result = (wakeup wake (); srv) >|= fun _ -> result in
  f () >>= stop_server
