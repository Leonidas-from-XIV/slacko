module Cohttp_unix = Cohttp_lwt_unix
module Cohttp_body = Cohttp_lwt_body

let base_url = "https://slack.com/api/"

let endpoint e =
  base_url ^ e
  |> Uri.of_string

let api_test ?foo () =
  let base = endpoint "api.test" in
  let uri = match foo with
    | None -> base
    | Some value -> Uri.add_query_param' base ("foo", value) in
  lwt (response, body) = Cohttp_unix.Client.get uri in
  Cohttp_body.to_string body

let auth_test token =
  let base = endpoint "auth.test" in
  let uri = Uri.add_query_param' base ("token", token) in
  lwt (response, body) = Cohttp_unix.Client.get uri in
  Cohttp_body.to_string body

(* internal *)
let add_optionally key value uri = match value with
  | None -> uri
  | Some value -> Uri.add_query_param' uri (key, value)

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
  lwt (response, body) = Cohttp_unix.Client.get uri in
  Cohttp_body.to_string body
