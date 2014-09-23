module Cohttp_unix = Cohttp_lwt_unix
module Cohttp_body = Cohttp_lwt_body

let base_url = "https://slack.com/api/"

let endpoint e =
  base_url ^ e
  |> Uri.of_string

(* internal *)
let optionally_add key value uri = match value with
  | None -> uri
  | Some value -> Uri.add_query_param' uri (key, value)

let definitely_add key value = optionally_add key (Some value)

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
      | `String "invalid_ts_latest" -> `Invalid_ts_latest
      | `String "invalid_ts_oldest" -> `Invalid_ts_oldest
      | _ -> `Error

(* filter out "ok" and "error" keys *)
let filter_useless = function
  | `Success `Assoc items -> `Success (
      `Assoc (List.filter (fun (k, _) -> k <> "ok" && k <> "error") items))
  | otherwise -> otherwise

let query uri =
  lwt (_, body) = Cohttp_unix.Client.get uri in
  lwt content = Cohttp_body.to_string body in
  Yojson.Basic.from_string content
    |> validate
    |> filter_useless
    |> Lwt.return

(* Slack API begins here *)

let api_test ?foo ?error () =
  let uri = endpoint "api.test"
    |> optionally_add "foo" foo
    |> optionally_add "error" error
  in query uri

let auth_test token =
  let uri = endpoint "auth.test"
    |> definitely_add "token" token
  in query uri

let channels_history token
  ?latest ?oldest ?count channel =
  let uri = endpoint "channels.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" latest
    |> optionally_add "oldest" oldest
    |> optionally_add "count" count
  in query uri

let channels_info token channel =
  let uri = endpoint "channels.info"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri

let channels_list ?exclude_archived token =
  let uri = endpoint "channels.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" exclude_archived
  in query uri

let chat_post_message token channel
  ?username ?parse ?icon_url ?icon_emoji text =
  let base = endpoint "chat.postMessage" in
  let required = Uri.add_query_params' base
    [("token", token);
     ("channel", channel);
     ("text", text)] in
  let uri = required
    |> optionally_add "username" username
    |> optionally_add "parse" parse
    |> optionally_add "icon_url" icon_url
    |> optionally_add "icon_emoji" icon_emoji
  in query uri
