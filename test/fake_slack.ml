(* A (currently pretty useless) fake slack implementation to run tests
   against. *)
open Lwt
open Cohttp_lwt_unix

(* The only "valid" token we accept. *)
let valid_token = "xoxp-testtoken"

(* The following values come from data captured by a relay proxy between slacko
   and slack.com. They need to be replaced with something more generic that
   doesn't depend on the arbitrary details of a particular slack team. *)
let ch_general = "C3UK9TS3C"
let ch_random = "C3TTWNCTA"
let ch_archivable = "C3XTJPLFL"
let ch_archived = "C3XTHDCTC"
let gr_seekrit = "G536YKXPE"
(* slacko doesn't have a lookup function for us, so we just use it directly. *)
let im_slackbot = "D3UMJU8VA"


let channels_json = Yojson.Safe.from_file "channels.json"
let new_channel_json = Yojson.Safe.from_file "new_channel.json"
let authed_json = Yojson.Safe.from_file "authed.json"
let random_history_json = Yojson.Safe.from_file "random_history.json"
let users_json = Yojson.Safe.from_file "users.json"
let files_json = Yojson.Safe.from_file "files.json"
let groups_json = Yojson.Safe.from_file "groups.json"
let seekrit_history_json = Yojson.Safe.from_file "seekrit_history.json"
let ims_json = Yojson.Safe.from_file "ims.json"
let slackbot_history_json = Yojson.Safe.from_file "slackbot_history.json"

let json_fields = function
  | `Assoc fields -> fields
  | _ -> failwith "Can't parse test json."


let reply_json ok fields =
  let body =
    `Assoc (("ok", `Bool ok) :: fields)
    |> Yojson.Safe.to_string
  in
  Server.respond_string ~status:`OK ~body ()

let reply_ok fields = reply_json true fields

let reply_err err fields = reply_json false (("error", `String err) :: fields)

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

let check_auth f req body =
  match get_arg "token" req with
  | t when t = valid_token -> f req body
  | _ -> reply_err "invalid_auth" []

(* Request handlers *)

let bad_path req _body =
  let path = req |> Request.uri |> Uri.path in
  reply_err "unknown_method" ["req_method", `String path]

let api_test req _body =
  let args = req |> Request.uri |> Uri.query in
  let field_of_arg (k, v) = k, `String (List.hd v) in
  let fields = match args with
    | [] -> []
    | args -> ["args", `Assoc (List.map field_of_arg args)]
  in
  match Uri.get_query_param (Request.uri req) "error" with
  | None -> reply_ok fields
  | Some err -> reply_err err fields

let auth_test _req _body =
  reply_ok (json_fields authed_json)

let channels_archive req _body =
  match get_arg "channel" req with
  | ch when ch = ch_general -> reply_err "cant_archive_general" []
  | ch when ch = ch_archivable -> reply_ok []
  | ch when ch = ch_archived -> reply_err "already_archived" []
  | _ -> reply_err "channel_not_found" []

let channels_create req _body =
  match get_arg "name" req with
  | "#general" | "#random" -> reply_err "name_taken" []
  | "#new_channel" | _ -> reply_ok ["channel", new_channel_json]

let channels_history req _body =
  (* TODO: Check various filtering params. *)
  match get_arg "channel" req with
  | ch when ch = ch_random -> reply_ok (json_fields random_history_json)
  | _ -> reply_err "channel_not_found" []

let channels_list _req _body =
  (* TODO: Check exclude_archived param. *)
  reply_ok ["channels", channels_json]

let files_list _req _body =
  (* TODO: Check various filtering params. *)
  reply_ok (json_fields files_json)

let groups_list _req _body =
  (* TODO: Check exclude_archived param. *)
  reply_ok ["groups", groups_json]

let groups_history req _body =
  (* TODO: Check various filtering params. *)
  match get_arg "channel" req with
  | gr when gr = gr_seekrit -> reply_ok (json_fields seekrit_history_json)
  | _ -> reply_err "channel_not_found" []

let im_list _req _body =
  reply_ok ["ims", ims_json]

let im_history req _body =
  (* TODO: Check various filtering params. *)
  match get_arg "channel" req with
  | im when im = im_slackbot -> reply_ok (json_fields slackbot_history_json)
  | _ -> reply_err "channel_not_found" []

let users_list _req _body =
  (* TODO: Check presence param. *)
  reply_ok (json_fields users_json)

(* Dispatcher, etc. *)

let server ?(port=7357) ~stop () =
  let callback _conn req body =
    let handler = match req |> Request.uri |> Uri.path with
      | "/api/api.test" -> api_test
      | "/api/auth.test" -> check_auth auth_test
      | "/api/channels.archive" -> check_auth channels_archive
      | "/api/channels.create" -> check_auth channels_create
      | "/api/channels.history" -> check_auth channels_history
      | "/api/channels.list" -> check_auth channels_list
      | "/api/files.list" -> check_auth files_list
      | "/api/groups.history" -> check_auth groups_history
      | "/api/groups.list" -> check_auth groups_list
      | "/api/im.history" -> check_auth im_history
      | "/api/im.list" -> check_auth im_list
      | "/api/users.list" -> check_auth users_list
      | _ -> bad_path
    in
    handler req body
  in
  Server.create ~mode:(`TCP (`Port port)) ~stop (Server.make ~callback ())

let with_fake_slack f =
  let stop, wake = wait () in
  let srv = server ~stop () in
  let stop_server _result =
    wakeup wake ();
    srv
  in
  finalize f stop_server
