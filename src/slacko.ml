module Cohttp_unix = Cohttp_lwt_unix
module Cohttp_body = Cohttp_lwt_body

let base_url = "https://slack.com/api/"

let endpoint e =
  base_url ^ e
  |> Uri.of_string

(* internal *)
let add_optionally key value uri = match value with
  | None -> uri
  | Some value -> Uri.add_query_param' uri (key, value)

let validate json =
  let open Yojson.Basic.Util in
  match json |> member "ok" |> to_bool with
    | true -> `Success json
    | false -> let error = json |> member "error" in
      match error with
      | `String "not_authed" -> `Not_authed
      | `String "invalid_auth" -> `Invalid_auth
      | `String "account_inactive" -> `Account_inactive
      | `String "channel_not_found" -> `Channel_not_found
      | `String "is_archived" -> `Is_archived
      | `String "msg_too_long" -> `Msg_too_long
      | `String "rate_limited" -> `Rate_limited
      | _ -> `Error

let query uri =
  lwt (_, body) = Cohttp_unix.Client.get uri in
  lwt content = Cohttp_body.to_string body in
  Yojson.Basic.from_string content
    |> validate
    |> Lwt.return

let api_test ?foo ?error () =
  let uri = endpoint "api.test"
    |> add_optionally "foo" foo
    |> add_optionally "error" error
  in
  query uri

let auth_test token =
  let base = endpoint "auth.test" in
  let uri = Uri.add_query_param' base ("token", token) in
  query uri

let chat_post_message token channel
  ?username ?parse ?icon_url ?icon_emoji text =
  let base = endpoint "chat.postMessage" in
  let required = Uri.add_query_params' base
    [("token", token);
     ("channel", channel);
     ("text", text)] in
  let uri = required
    |> add_optionally "username" username
    |> add_optionally "parse" parse
    |> add_optionally "icon_url" icon_url
    |> add_optionally "icon_emoji" icon_emoji
  in
  query uri
