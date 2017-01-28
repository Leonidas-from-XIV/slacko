(* A (currently pretty useless) fake slack implementation to run tests
   against. *)
open Lwt
open Cohttp
open Cohttp_lwt_unix


let channels_json = Yojson.Safe.from_file "channels.json"
let authed_json = Yojson.Safe.from_file "authed.json"

let json_fields = function
  | `Assoc fields -> fields
  | _ -> failwith "Can't parse test json."


let resp ok fields =
  let body = `Assoc (("ok", `Bool ok) :: fields) |> Yojson.Safe.to_string in
  Server.respond_string ~status:`OK ~body ()

let ok_resp fields = resp true fields

let err_resp err fields = resp false (("error", `String err) :: fields)

let get_arg_opt arg req =
  Uri.get_query_param (Request.uri req) arg

let get_arg_default arg default req =
  match get_arg_opt arg req with
  | Some x -> x
  | None -> default

let get_arg arg req =
  match get_arg_opt arg req with
  | Some x -> x
  | None -> failwith @@ "Mandatory arg " ^ arg ^ " not given."

(* Request handlers *)

let bad_path req body =
  let path = req |> Request.uri |> Uri.path in
  err_resp "unknown_method" ["req_method", `String path]

let api_test req body =
  let args = req |> Request.uri |> Uri.query in
  let field_of_arg (k, v) = k, `String (List.hd v) in
  let fields = match args with
    | [] -> []
    | args -> ["args", `Assoc (List.map field_of_arg args)]
  in
  match Uri.get_query_param (Request.uri req) "error" with
  | None -> ok_resp fields
  | Some err -> err_resp err fields

let auth_test req body =
  match get_arg "token" req with
  | "xoxp-testtoken" -> ok_resp (json_fields authed_json)
  | _ -> err_resp "invalid_auth" []

let channels_list req body =
  ok_resp ["channels", channels_json]

(* Dispatcher, etc. *)

let server ?(port=7357) ~stop () =
  let callback _conn req body =
    let handler = match req |> Request.uri |> Uri.path with
      | "/api/api.test" -> api_test
      | "/api/auth.test" -> auth_test
      | "/api/channels.list" -> channels_list
      | _ -> bad_path
    in
    handler req body
  in
  Server.create ~mode:(`TCP (`Port port)) ~stop (Server.make ~callback ())

let with_fake_slack f =
  let stop, wake = wait () in
  let srv = server ~stop () in
  let stop_server result = wakeup wake (); srv in
  finalize f stop_server
