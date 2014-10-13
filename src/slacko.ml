(*
* Slacko - Binding to the Slack API
* Copyright (C) 2014 Marek Kubica <marek@xivilization.net>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 3.0 of the License, or (at your option) any later version,
* with the special exception on linking described in file COPYING.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*)

module Cohttp_unix = Cohttp_lwt_unix
module Cohttp_body = Cohttp_lwt_body

type api_result = [
  | `Success of Yojson.Basic.json
  | `Unknown_error
]

type auth_error = [
  | `Not_authed
  | `Invalid_auth
  | `Account_inactive
]

type timestamp_error = [
  | `Invalid_ts_latest
  | `Invalid_ts_oldest
]

type channel_error = [
  | `Channel_not_found
]

type user_error = [
  | `User_not_found
]

type invite_error = [
  | `Cant_invite_self
  | `Cant_invite
]

type not_in_channel_error = [
  | `Not_in_channel
]

type already_in_channel_error = [
  | `Already_in_channel
]

type archive_error = [
  | `Is_archived
]

type name_error = [
  | `Name_taken
]

type kick_error = [
  | `Cant_kick_self
]

type channel_kick_error = [
  | kick_error
  | `Cant_kick_from_general
  | `Cant_kick_from_last_channel
]

type restriction_error = [
  | `Restricted_action
]

type leave_general_error = [
  | `Cant_leave_general
]

type topic_error = [
  | `Too_long
]

type message_error = [
  | `Cant_delete_message
  | `Message_not_found
]

type message_length_error = [
  | `Msg_too_long
]

type rate_error = [
  | `Rate_limited
]

type message_update_error = [
  | `Message_not_found
  | `Cant_update_message
  | `Edit_window_closed
]

type file_error = [
  | `File_not_found
  | `File_deleted
]

type unknown_type_error = [
  | `Unknown_type
]

type already_archived_error = [
  | `Already_archived
]

type not_in_group_error = [
  | `Not_in_group
]

type leave_last_channel_error = [
  | `Cant_leave_last_channel
]

type last_member_error = [
  | `Last_member
]

type oauth_error = [
  | `Invalid_client_id
  | `Bad_client_secret
  | `Invalid_code
  | `Bad_redirect_uri
]

type presence_error = [
  | `Invalid_presence
]

type user_visibility_error = [
  | `User_not_visible
]

type authed_result = [
  | api_result
  | auth_error
]

type purpose_result = [
  | authed_result
  | channel_error
  | archive_error
  | not_in_channel_error
  | topic_error
]

type history_result = [
  | authed_result
  | channel_error
  | timestamp_error
]

type timestamp = float

type token = string

type topic = string

type message = string

type channel = ChannelId of string | ChannelName of string

type user = UserId of string | UserName of string

(* TODO make this an union type *)
type group = string

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
      | `String "account_inactive" -> `Account_inactive
      | `String "already_archived" -> `Already_archived
      | `String "already_in_channel" -> `Already_in_channel
      | `String "bad_client_secret" -> `Bad_client_secret
      | `String "bad_redirect_uri" -> `Bad_redirect_uri
      | `String "cant_invite" -> `Cant_invite
      | `String "cant_invite_self" -> `Cant_invite_self
      | `String "cant_delete_message" -> `Cant_delete_message
      | `String "cant_kick_from_general" -> `Cant_kick_from_general
      | `String "cant_kick_from_last_channel" -> `Cant_kick_from_last_channel
      | `String "cant_kick_self" -> `Cant_kick_self
      | `String "cant_leave_general" -> `Cant_leave_general
      | `String "cant_leave_last_channel" -> `Cant_leave_last_channel
      | `String "cant_update_message" -> `Cant_update_message
      | `String "channel_not_found" -> `Channel_not_found
      | `String "edit_window_closed" -> `Edit_window_closed
      | `String "file_deleted" -> `File_deleted
      | `String "file_not_found" -> `File_not_found
      | `String "invalid_auth" -> `Invalid_auth
      | `String "invalid_client_id" -> `Invalid_client_id
      | `String "invalid_code" -> `Invalid_code
      | `String "invalid_presence" -> `Invalid_presence
      | `String "invalid_ts_latest" -> `Invalid_ts_latest
      | `String "invalid_ts_oldest" -> `Invalid_ts_oldest
      | `String "is_archived" -> `Is_archived
      | `String "last_member" -> `Last_member
      | `String "message_not_found" -> `Message_not_found
      | `String "msg_too_long" -> `Msg_too_long
      | `String "name_taken" -> `Name_taken
      (* can't really happen *)
      | `String "no_channel" -> `No_channel
      (* can't really happen either *)
      | `String "no_text" -> `No_text
      | `String "not_authed" -> `Not_authed
      | `String "not_in_channel" -> `Not_in_channel
      | `String "rate_limited" -> `Rate_limited
      | `String "restricted_action" -> `Restricted_action
      | `String "too_long" -> `Too_long
      | `String "unknown_type" -> `Unknown_type
      | `String "user_not_found" -> `User_not_found
      | `String "user_not_visible" -> `User_not_visible
      | _ -> `Unknown_error

(* filter out "ok" and "error" keys *)
let filter_useless = function
  | `Success `Assoc items -> `Success (
      `Assoc (List.filter (fun (k, _) -> k <> "ok" && k <> "error") items))
  | otherwise -> otherwise

let query uri return_value_fn =
  lwt (_, body) = Cohttp_unix.Client.get uri in
  lwt content = Cohttp_body.to_string body in
  Yojson.Basic.from_string content
    |> validate
    |> filter_useless
    |> return_value_fn
    |> Lwt.return

(* do a POST request *)
let query_post uri body return_value_fn =
  lwt (_, body) = Cohttp_unix.Client.post ~body uri in
  lwt content = Cohttp_body.to_string body in
  Yojson.Basic.from_string content
    |> validate
    |> filter_useless
    |> return_value_fn
    |> Lwt.return

(* like string_of_float, but doesn't truncate numbers to end with '.',
 * e.g. '42.' *)
let string_of_timestamp = Printf.sprintf "%.f"
let identity x = x
(* Many functions can only succeed or fail due to an auth error *)
let only_auth_can_fail = function
  | #authed_result as res -> res
  | other -> `Unknown_error

(* Slacko API helper methods *)
let token_of_string = identity
let group_of_string = identity
(* TODO: should this do validation of the mesages? Length limits? *)
let message_of_string = identity
let topic_of_string = identity

let channel_of_string s =
  if s.[0] = 'C' then ChannelId s else ChannelName s

let user_of_string s =
  if s.[0] = 'U' then UserId s else UserName s

(* Slack API begins here *)

let api_test ?foo ?error () =
  let uri = endpoint "api.test"
    |> optionally_add "foo" foo
    |> optionally_add "error" error
  in query uri (function
    | #api_result as res -> res
    | other -> `Unknown_error)

let auth_test token =
  let uri = endpoint "auth.test"
    |> definitely_add "token" token
  in query uri only_auth_can_fail

let channels_history token
  ?latest ?oldest ?count channel =
  let uri = endpoint "channels.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" latest
    |> optionally_add "oldest" oldest
    |> optionally_add "count" count
  in query uri (function
    | #history_result as res -> res
    | other -> `Unknown_error)

let channels_info token channel =
  let uri = endpoint "channels.info"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | other -> `Unknown_error)

let channels_invite token channel user =
  let uri = endpoint "channels.info"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #user_error as err -> err
    | #invite_error as err -> err
    | #not_in_channel_error as err -> err
    | #already_in_channel_error as err -> err
    | #archive_error as err -> err
    | other -> `Unknown_error)

let channels_join token name =
  let uri = endpoint "channels.join"
    |> definitely_add "token" token
    |> definitely_add "name" name
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #name_error as err -> err
    | #archive_error as err -> err
    | other -> `Unknown_error)

let channels_kick token channel user =
  let uri = endpoint "channels.kick"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #user_error as err -> err
    | #channel_kick_error as err -> err
    | #not_in_channel_error as err -> err
    | #restriction_error as err -> err
    | other -> `Unknown_error)

let channels_leave token channel =
  let uri = endpoint "channels.leave"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #archive_error as err -> err
    | #leave_general_error as err -> err
    | other -> `Unknown_error)

let channels_list ?exclude_archived token =
  let uri = endpoint "channels.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" exclude_archived
  in query uri only_auth_can_fail

let channels_mark token channel ts =
  let uri = endpoint "channels.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" ts
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #archive_error as err -> err
    | #not_in_channel_error as err -> err
    | other -> `Unknown_error)

let channels_set_purpose token channel purpose =
  let uri = endpoint "channels.setPurpose"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "purpose" purpose
  in query uri (function
    | #purpose_result as res -> res
    | other -> `Unknown_error)

let channels_set_topic token channel topic =
  let uri = endpoint "channels.setTopic"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "topic" topic
  in query uri (function
    | #purpose_result as res -> res
    | other -> `Unknown_error)

let chat_delete token ts channel =
  let uri = endpoint "chat.delete"
    |> definitely_add "token" token
    |> definitely_add "ts" ts
    |> definitely_add "channel" channel
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #message_error as err -> err
    | other -> `Unknown_error)

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
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #archive_error as err -> err
    | #message_length_error as err -> err
    | #rate_error as err -> err
    | other -> `Unknown_error)

let chat_update token ts channel text =
  let uri = endpoint "chat.update"
    |> definitely_add "token" token
    |> definitely_add "ts" ts
    |> definitely_add "channel" channel
    |> definitely_add "text" text
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #message_update_error as err -> err
    | #message_length_error as err -> err
    | other -> `Unknown_error)

let emoji_list token =
  let uri = endpoint "emoji.list"
    |> definitely_add "token" token
  in query uri only_auth_can_fail

let files_info token ?count ?page file =
  let uri = endpoint "files.info"
    |> definitely_add "token" token
    |> definitely_add "file" file
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri (function
    | #authed_result as res -> res
    | #file_error as err -> err
    | other -> `Unknown_error)

let files_list ?user ?ts_from ?ts_to ?types ?count ?page token =
  let uri = endpoint "files.list"
    |> definitely_add "token" token
    |> optionally_add "user" user
    |> optionally_add "ts_from" ts_from
    |> optionally_add "ts_to" ts_to
    |> optionally_add "types" types
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri (function
    | #authed_result as res -> res
    | #user_error as err -> err
    | #unknown_type_error as err -> err
    | other -> `Unknown_error)

let files_upload token
  ?filetype ?filename ?title ?initial_comment ?channels content =
  let uri = endpoint "files.upload"
    |> definitely_add "token" token
    |> optionally_add "filetype" filetype
    |> optionally_add "filename" filename
    |> optionally_add "title" title
    |> optionally_add "initial_comment" initial_comment
    |> optionally_add "channels" channels
  in query_post uri content only_auth_can_fail

let groups_create token name =
  let uri = endpoint "groups.create"
    |> definitely_add "token" token
    |> definitely_add "name" name
  in query uri (function
    | #authed_result as res -> res
    | #name_error as err -> err
    | #restriction_error as err -> err
    | other -> `Unknown_error)

let groups_create_child token channel =
  let uri = endpoint "groups.createChild"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #already_archived_error as err -> err
    | #restriction_error as err -> err
    | other -> `Unknown_error)

let groups_history token ?latest ?oldest ?count channel =
  let uri = endpoint "groups.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" latest
    |> optionally_add "oldest" oldest
    |> optionally_add "count" count
  in query uri (function
    | #history_result as res -> res
    | other -> `Unknown_error)

let groups_invite token channel user =
  let uri = endpoint "groups.invite"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #user_error as err -> err
    | #invite_error as err -> err
    | #archive_error as err -> err
    | other -> `Unknown_error)

let groups_kick token channel user =
  let uri = endpoint "groups.kick"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #user_error as err -> err
    | #kick_error as err -> err
    | #not_in_group_error as err -> err
    | #restriction_error as err -> err
    | other -> `Unknown_error)

let groups_leave token channel =
  let uri = endpoint "groups.leave"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #archive_error as err -> err
    | #leave_last_channel_error as err -> err
    | #last_member_error as err -> err
    | other -> `Unknown_error)

let groups_list ?exclude_archived token =
  let uri = endpoint "groups.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" exclude_archived
  in query uri only_auth_can_fail

let groups_mark token channel ts =
  let uri = endpoint "groups.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" ts
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #archive_error as err -> err
    | #not_in_channel_error as err -> err
    | other -> `Unknown_error)

let groups_set_purpose token channel purpose =
  let uri = endpoint "groups.setPurpose"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "purpose" purpose
  in query uri (function
    | #purpose_result as res -> res
    | other -> `Unknown_error)

let groups_set_topic token channel topic =
  let uri = endpoint "groups.setTopic"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "topic" topic
  in query uri (function
    | #purpose_result as res -> res
    | other -> `Unknown_error)

let im_history token ?latest ?oldest ?count channel =
  let uri = endpoint "im.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" latest
    |> optionally_add "oldest" oldest
    |> optionally_add "count" count
  in query uri (function
    | #history_result as res -> res
    | other -> `Unknown_error)

let im_list token =
  let uri = endpoint "im.list"
    |> definitely_add "token" token
  in query uri only_auth_can_fail

let im_mark token channel ts =
  let uri = endpoint "im.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" ts
  in query uri (function
    | #authed_result as res -> res
    | #channel_error as err -> err
    | #not_in_channel_error as err -> err
    | other -> `Unknown_error)

let oauth_access client_id client_secret ?redirect_url code =
  let uri = endpoint "oauth.access"
    |> definitely_add "client_id" client_id
    |> definitely_add "client_secret" client_secret
    |> definitely_add "code" code
    |> optionally_add "redirect_url" redirect_url
  in query uri (function
    | #api_result as res -> res
    | #oauth_error as err -> err
    | other -> `Unknown_error)

let presence_set token presence =
  let uri = endpoint "presence.set"
    |> definitely_add "token" token
    |> definitely_add "presence" presence
  in query uri (function
    | #authed_result as res -> res
    | #presence_error as err -> err
    | other -> `Unknown_error)

let search base token ?sort ?sort_dir ?highlight ?count ?page query_ =
  let uri = base
    |> definitely_add "token" token
    |> definitely_add "query" query_
    |> optionally_add "sort" sort
    |> optionally_add "sort_dir" sort_dir
    |> optionally_add "highlight" highlight
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri only_auth_can_fail

let search_all = search @@ endpoint "search.all"
let search_files = search @@ endpoint "search.files"
let search_messages = search @@ endpoint "search.messages"

let stars_list ?user ?count ?page token =
  let uri = endpoint "stars.list"
    |> definitely_add "token" token
    |> optionally_add "user" user
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri (function
    | #authed_result as res -> res
    | #user_error as err -> err
    | other -> `Unknown_error)

let users_info token user =
  let uri = endpoint "users.info"
    |> definitely_add "token" token
    |> definitely_add "user" user
  in query uri (function
    | #authed_result as res -> res
    | #user_error as err -> err
    | #user_visibility_error as err -> err
    | other -> `Unknown_error)

let users_list token =
  let uri = endpoint "users.list"
    |> definitely_add "token" token
  in query uri only_auth_can_fail

let users_set_active token =
  let uri = endpoint "users.setActive"
    |> definitely_add "token" token
  in query uri only_auth_can_fail
