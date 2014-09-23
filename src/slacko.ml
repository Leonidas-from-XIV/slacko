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
      | `String "user_not_found" -> `User_not_found
      | `String "cant_invite_self" -> `Cant_invite_self
      | `String "not_in_channel" -> `Not_in_channels
      | `String "already_in_channel" -> `Already_in_channel
      | `String "cant_invite" -> `Cant_invite
      | `String "name_taken" -> `Name_taken
      (* can't really happen *)
      | `String "no_channel" -> `No_channel
      | `String "cant_kick_self" -> `Cant_kick_self
      | `String "cant_kick_from_general" -> `Cant_kick_from_general
      | `String "cant_kick_from_last_channel" -> `Cant_kick_from_last_channel
      | `String "restricted_action" -> `Restricted_action
      | `String "cant_leave_general" -> `Cant_leave_general
      | `String "too_long" -> `Too_long
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

let channels_invite token channel user =
  let uri = endpoint "channels.info"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri

let channels_join token name =
  let uri = endpoint "channels.join"
    |> definitely_add "token" token
    |> definitely_add "name" name
  in query uri

let channels_kick token channel user =
  let uri = endpoint "channels.kick"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri

let channels_leave token channel =
  let uri = endpoint "channels.leave"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri

let channels_list ?exclude_archived token =
  let uri = endpoint "channels.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" exclude_archived
  in query uri

let channels_mark token channel ts =
  let uri = endpoint "channels.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" ts
  in query uri

let channels_set_purpose token channel purpose =
  let uri = endpoint "channels.setPurpose"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "purpose" purpose
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
