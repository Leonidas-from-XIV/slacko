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
  | `Unhandled_error of string
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

type invalid_name_error = [
  | `Invalid_name
]

type authed_result = [
  | api_result
  | auth_error
]

type topic_result = [
  | authed_result
  | channel_error
  | archive_error
  | not_in_channel_error
  | topic_error
  | `User_is_restricted
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

type group = GroupId of string | GroupName of string

type sort_criterion = Score | Timestamp

type sort_direction = Ascending | Descending

type presence = Active | Away

(* some useful Lwt operators *)
let (>|=) = Lwt.(>|=)
let (>>=) = Lwt.(>>=)

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
      | `String "cant_archive_general" -> `Cant_archive_general
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
      | `String "last_ra_channel" -> `Last_restricted_channel
      | `String "message_not_found" -> `Message_not_found
      (* not supposed to happen *)
      | `String "msg_too_long" -> `Msg_too_long
      | `String "name_taken" -> `Name_taken
      (* can't really happen *)
      | `String "no_channel" -> `No_channel
      (* can't really happen either *)
      | `String "no_text" -> `No_text
      | `String "not_archived" -> `Not_archived
      | `String "not_authed" -> `Not_authed
      | `String "not_in_channel" -> `Not_in_channel
      | `String "rate_limited" -> `Rate_limited
      | `String "restricted_action" -> `Restricted_action
      | `String "too_long" -> `Too_long
      | `String "unknown_type" -> `Unknown_type
      | `String "user_not_found" -> `User_not_found
      | `String "user_not_visible" -> `User_not_visible
      | `String "not_authorized" -> `Not_authorized
      | `String "invalid_name" -> `Invalid_name
      | `String "user_is_restricted" -> `User_is_restricted
      (* lolwat, I'm not making this up *)
      | `String "user_is_ultra_restricted" -> `User_is_ultra_restricted
      | `String "user_does_not_own_channel" -> `User_does_not_own_channel
      (* when the API changes and introduces new, yet unhandled error types *)
      | `String err -> `Unhandled_error err
      | _ -> `Unknown_error

(* filter out "ok" and "error" keys *)
let filter_useless = function
  | `Success `Assoc items -> `Success (
      `Assoc (List.filter (fun (k, _) -> k <> "ok" && k <> "error") items))
  | otherwise -> otherwise

let query return_value_fn uri =
  let%lwt (_, body) = Cohttp_unix.Client.get uri in
  let%lwt content = Cohttp_body.to_string body in
  Yojson.Basic.from_string content
    |> validate
    |> filter_useless
    |> return_value_fn
    |> Lwt.return

(* do a POST request *)
let query_post body return_value_fn uri =
  let%lwt (_, body) = Cohttp_unix.Client.post ~body uri in
  let%lwt content = Cohttp_body.to_string body in
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
  | _ -> `Unknown_error

let maybe fn = function
  | Some v -> Some (fn v)
  | None -> None

let channels_list ?exclude_archived token =
  endpoint "channels.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" @@ maybe string_of_bool @@ exclude_archived
    |> query only_auth_can_fail

let users_list token =
  endpoint "users.list"
    |> definitely_add "token" token
    |> query only_auth_can_fail

let groups_list ?exclude_archived token =
  endpoint "groups.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" @@ maybe string_of_bool exclude_archived
    |> query only_auth_can_fail

exception No_matches
exception No_unique_matches
exception Lookup_failed

(* look up the id of query from results provided by the listfn *)
let lookup token listfn collection query =
  let open Yojson.Basic.Util in
  match%lwt listfn token with
  | `Success json -> (let candidates = json |> member collection |> to_list |>
    filter_map (fun chan ->
      match (chan |> member "name") with
        (* If a channel matches the name, get its ID *)
        | `String q when q = query -> Some (chan |> member "id" |> to_string)
        | _ -> None) in
    (* make sure we have only one candidate *)
    match candidates with
      | [] -> Lwt.fail No_matches
      | [x] -> Lwt.return x
      | _ -> Lwt.fail No_unique_matches)
  | _ -> Lwt.fail Lookup_failed

let id_of_channel token = function
  | ChannelId id -> Lwt.return id
  | ChannelName name -> lookup token channels_list "channels" @@
      (* Split off the leading '#' *)
      String.sub name 1 @@ String.length name - 1

(* like id_of_channel but does not resolve names to ids *)
let string_of_channel = function
  | ChannelId id -> id
  | ChannelName name -> name

let id_of_user token = function
  | UserId id -> Lwt.return id
  | UserName name -> lookup token users_list "members" name

let id_of_group token = function
  | GroupId id -> Lwt.return id
  | GroupName name -> lookup token groups_list "groups" name

let name_of_group = function
  | GroupId id -> failwith "Need to specify a name"
  | GroupName name -> name

let string_of_bool = function
  | true -> "1"
  | false -> "0"

let string_of_criterion = function
  | Score -> "score"
  | Timestamp -> "timestamp"

let string_of_direction = function
  | Ascending -> "asc"
  | Descending -> "desc"

let string_of_presence = function
  | Active -> "active"
  | Away -> "away"

(* Slacko API helper methods *)
let token_of_string = identity
let message_of_string = identity

(* Calculate the amount of codepoints in a string encoded in UTF-8 *)
let utf8_codepoints text =
  (* convert string to int list *)
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (Char.code s.[i] :: l) in
    exp (String.length s - 1) [] in
  (*
   * http://www.daemonology.net/blog/2008-06-05-faster-utf8-strlen.html
   * http://porg.es/blog/counting-characters-in-utf-8-strings-is-faster
   *)
  let rec codepoints = function
    | [] -> 0
    | x::xs when x < 0x7F -> 1 + codepoints xs
    | x::_::xs when x >= 0xC0 && x <= 0xDF -> 1 + codepoints xs
    | x::_::_::xs when x >= 0xE0 && x <= 0xEF -> 1 + codepoints xs
    | x::_::_::_::xs when x >= 0xF0 && x <= 0xFF -> 1 + codepoints xs
    (* you are bad and should feel bad *)
    | x::_ -> failwith @@ Printf.sprintf "Invalid UTF-8 byte: 0x%X" x in
  codepoints @@ explode text

let topic_of_string text = if utf8_codepoints text <= 250 then Some text else None

let topic_of_string_exn text =
  match topic_of_string text with
  | Some t -> t
  | None -> failwith "Too long"

let channel_of_string s =
  if s.[0] = 'C' then ChannelId s else ChannelName s

let user_of_string s =
  if s.[0] = 'U' then UserId s else UserName s

let group_of_string s =
  if s.[0] = 'G' then GroupId s else GroupName s

(* Slack API begins here *)

let api_test ?foo ?error () =
  endpoint "api.test"
    |> optionally_add "foo" foo
    |> optionally_add "error" error
    |> query (function
    | #api_result as res -> res
    | _ -> `Unknown_error)

let auth_test token =
  endpoint "auth.test"
    |> definitely_add "token" token
    |> query only_auth_can_fail

let channels_archive token channel =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.archive"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> query (function
    | #authed_result
    | #channel_error
    | #already_archived_error
    | `Cant_archive_general
    | `Last_restricted_channel
    | #restriction_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let channels_create token name =
  endpoint "channels.create"
    |> definitely_add "token" token
    |> definitely_add "name" name
    |> query (function
    | #authed_result
    | #name_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let channels_history token
  ?latest ?oldest ?count channel =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> optionally_add "latest" @@ maybe string_of_timestamp latest
    |> optionally_add "oldest" @@ maybe string_of_timestamp oldest
    |> optionally_add "count" @@ maybe string_of_int count
    |> query (function
    | #history_result as res -> res
    | _ -> `Unknown_error)

let channels_info token channel =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.info"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> query (function
    | #authed_result
    | #channel_error as res -> res
    | _ -> `Unknown_error)

let channels_invite token channel user =
  let%lwt channel_id = id_of_channel token channel and
    user_id = id_of_user token user in
  endpoint "channels.info"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> definitely_add "user" user_id
    |> query (function
    | #authed_result
    | #channel_error
    | #user_error
    | #invite_error
    | #not_in_channel_error
    | #already_in_channel_error
    | #archive_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error)

let channels_join token name =
  endpoint "channels.join"
    |> definitely_add "token" token
    |> definitely_add "name" @@ string_of_channel name
    |> query (function
    | #authed_result
    | #channel_error
    | #name_error
    | #archive_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let channels_kick token channel user =
  let%lwt channel_id = id_of_channel token channel and
    user_id = id_of_user token user in
  endpoint "channels.kick"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> definitely_add "user" user_id
    |> query (function
    | #authed_result
    | #channel_error
    | #user_error
    | #channel_kick_error
    | #not_in_channel_error
    | #restriction_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let channels_leave token channel =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.leave"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> query (function
    | #authed_result
    | #channel_error
    | #archive_error
    | #leave_general_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let channels_mark token channel ts =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> definitely_add "ts" @@ string_of_timestamp ts
    |> query (function
    | #authed_result
    | #channel_error
    | #archive_error
    | #not_in_channel_error as res -> res
    | _ -> `Unknown_error)

let channels_rename token channel name =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.rename"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> definitely_add "name" name
    |> query (function
    | #authed_result
    | #channel_error
    | #not_in_channel_error
    | #name_error
    | #invalid_name_error
    | `Not_authorized
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let channels_set_purpose token channel purpose =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.setPurpose"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> definitely_add "purpose" purpose
    |> query (function
    | #topic_result as res -> res
    | _ -> `Unknown_error)

let channels_set_topic token channel topic =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.setTopic"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> definitely_add "topic" topic
    |> query (function
    | #topic_result as res -> res
    | _ -> `Unknown_error)

let channels_unarchive token channel =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "channels.unarchive"
    |> definitely_add "token" token
    |> definitely_add "channel" channel_id
    |> query (function
    | #authed_result
    | #channel_error
    | `Not_archived
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let chat_delete token ts channel =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "chat.delete"
    |> definitely_add "token" token
    |> definitely_add "ts" @@ string_of_timestamp ts
    |> definitely_add "channel" channel_id
    |> query (function
    | #authed_result
    | #channel_error
    | #message_error as res -> res
    | _ -> `Unknown_error)

let chat_post_message token channel
  ?username ?parse ?icon_url ?icon_emoji text =
  endpoint "chat.postMessage"
    |> definitely_add "token" token
    |> definitely_add "channel" @@ string_of_channel channel
    |> definitely_add "text" text
    |> optionally_add "username" username
    |> optionally_add "parse" parse
    |> optionally_add "icon_url" icon_url
    |> optionally_add "icon_emoji" icon_emoji
    |> query (function
    | #authed_result
    | #channel_error
    | #archive_error
    | #message_length_error
    | #rate_error as res -> res
    | _ -> `Unknown_error)

let chat_update token ts channel text =
  let%lwt channel_id = id_of_channel token channel in
  endpoint "chat.update"
    |> definitely_add "token" token
    |> definitely_add "ts" @@ string_of_timestamp ts
    |> definitely_add "channel" channel_id
    |> definitely_add "text" text
    |> query (function
    | #authed_result
    | #channel_error
    | #message_update_error
    | #message_length_error as res -> res
    | _ -> `Unknown_error)

let emoji_list token =
  endpoint "emoji.list"
    |> definitely_add "token" token
    |> query only_auth_can_fail

let files_info token ?count ?page file =
  endpoint "files.info"
    |> definitely_add "token" token
    |> definitely_add "file" file
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query (function
    | #authed_result
    | #file_error as res -> res
    | _ -> `Unknown_error)

let files_list ?user ?ts_from ?ts_to ?types ?count ?page token =
  let%lwt user_id = match user with
    | Some u -> id_of_user token u >|= (fun x -> Some x)
    | None -> Lwt.return None in
  endpoint "files.list"
    |> definitely_add "token" token
    |> optionally_add "user" user_id
    |> optionally_add "ts_from" @@ maybe string_of_timestamp ts_from
    |> optionally_add "ts_to" @@ maybe string_of_timestamp ts_to
    |> optionally_add "types" types
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query (function
    | #authed_result
    | #user_error
    | #unknown_type_error as res -> res
    | _ -> `Unknown_error)

let files_upload token
  ?filetype ?filename ?title ?initial_comment ?channels content =
  endpoint "files.upload"
    |> definitely_add "token" token
    |> optionally_add "filetype" filetype
    |> optionally_add "filename" filename
    |> optionally_add "title" title
    |> optionally_add "initial_comment" initial_comment
    |> optionally_add "channels" channels
    |> query_post content only_auth_can_fail

let groups_archive token group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.archive"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> query (function
    | #authed_result
    | #channel_error
    | #already_archived_error
    | `Group_contains_others
    | `Last_restricted_channel
    | #restriction_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error)

let groups_close token group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.close"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> query (function
    | #authed_result
    | #channel_error as res -> res
    | _ -> `Unknown_error)

let groups_create token name =
  endpoint "groups.create"
    |> definitely_add "token" token
    |> definitely_add "name" @@ name_of_group name
    |> query (function
    | #authed_result
    | #name_error
    | #restriction_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error)

let groups_create_child token group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.createChild"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> query (function
    | #authed_result
    | #channel_error
    | #already_archived_error
    | #restriction_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error)

let groups_history token ?latest ?oldest ?count group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.history"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> optionally_add "latest" @@ maybe string_of_timestamp latest
    |> optionally_add "oldest" @@ maybe string_of_timestamp oldest
    |> optionally_add "count" @@ maybe string_of_int count
    |> query (function
    | #history_result as res -> res
    | _ -> `Unknown_error)

let groups_invite token group user =
  let%lwt user_id = id_of_user token user and
    group_id = id_of_group token group in
  endpoint "groups.invite"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> definitely_add "user" user_id
    |> query (function
    | #authed_result
    | #channel_error
    | #user_error
    | #invite_error
    | #archive_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error)

let groups_kick token group user =
  let%lwt user_id = id_of_user token user and
    group_id = id_of_group token group in
  endpoint "groups.kick"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> definitely_add "user" user_id
    |> query (function
    | #authed_result
    | #channel_error
    | #user_error
    | #kick_error
    | #not_in_group_error
    | #restriction_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let groups_leave token group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.leave"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> query (function
    | #authed_result
    | #channel_error
    | #archive_error
    | #leave_last_channel_error
    | #last_member_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error)

let groups_mark token group ts =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> definitely_add "ts" @@ string_of_timestamp ts
    |> query (function
    | #authed_result
    | #channel_error
    | #archive_error
    | #not_in_channel_error as res -> res
    | _ -> `Unknown_error)

let groups_open token group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.open"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> query (function
    | #authed_result
    | #channel_error as res -> res
    | _ -> `Unknown_error)

let groups_rename token group name =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.rename"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> definitely_add "name" name
    |> query (function
    | #authed_result
    | #channel_error
    | #name_error
    | #invalid_name_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let groups_set_purpose token group purpose =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.setPurpose"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> definitely_add "purpose" purpose
    |> query (function
    | #topic_result as res -> res
    | _ -> `Unknown_error)

let groups_set_topic token group topic =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.setTopic"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> definitely_add "topic" topic
    |> query (function
    | #topic_result as res -> res
    | _ -> `Unknown_error)

let groups_unarchive token group =
  let%lwt group_id = id_of_group token group in
  endpoint "groups.unarchive"
    |> definitely_add "token" token
    |> definitely_add "channel" group_id
    |> query (function
    | #authed_result
    | #channel_error
    | `Not_archived
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error)

let im_close token channel =
  endpoint "im.close"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> query (function
    | #authed_result
    | #channel_error
    | `User_does_not_own_channel as res -> res
    | _ -> `Unknown_error)

let im_history token ?latest ?oldest ?count channel =
  endpoint "im.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" @@ maybe string_of_timestamp latest
    |> optionally_add "oldest" @@ maybe string_of_timestamp oldest
    |> optionally_add "count" @@ maybe string_of_int count
    |> query (function
    | #history_result as res -> res
    | _ -> `Unknown_error)

let im_list token =
  endpoint "im.list"
    |> definitely_add "token" token
    |> query only_auth_can_fail

let im_mark token channel ts =
  endpoint "im.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" @@ string_of_timestamp ts
    |> query (function
    | #authed_result
    | #channel_error
    | #not_in_channel_error as res -> res
    | _ -> `Unknown_error)

let im_open token user =
  let%lwt user_id = id_of_user token user in
  endpoint "im.open"
    |> definitely_add "token" token
    |> definitely_add "user" user_id
    |> query (function
    | #authed_result
    | #user_error
    | #user_visibility_error as res -> res
    | _ -> `Unknown_error)

let oauth_access client_id client_secret ?redirect_url code =
  endpoint "oauth.access"
    |> definitely_add "client_id" client_id
    |> definitely_add "client_secret" client_secret
    |> definitely_add "code" code
    |> optionally_add "redirect_url" redirect_url
    |> query (function
    | #api_result
    | #oauth_error as res -> res
    | _ -> `Unknown_error)

let presence_set token presence =
  endpoint "presence.set"
    |> definitely_add "token" token
    |> definitely_add "presence" @@ string_of_presence presence
    |> query (function
    | #authed_result
    | #presence_error as res -> res
    | _ -> `Unknown_error)

let search base token ?sort ?sort_dir ?highlight ?count ?page query_ =
  base
    |> definitely_add "token" token
    |> definitely_add "query" query_
    |> optionally_add "sort" @@ maybe string_of_criterion sort
    |> optionally_add "sort_dir" @@ maybe string_of_direction sort_dir
    |> optionally_add "highlight" @@ maybe string_of_bool highlight
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query only_auth_can_fail

let search_all = search @@ endpoint "search.all"
let search_files = search @@ endpoint "search.files"
let search_messages = search @@ endpoint "search.messages"

let stars_list ?user ?count ?page token =
  let%lwt user_id = match user with
    | Some u -> id_of_user token u >|= (fun x -> Some x)
    | None -> Lwt.return None in
  endpoint "stars.list"
    |> definitely_add "token" token
    |> optionally_add "user" user_id
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query (function
    | #authed_result
    | #user_error as res -> res
    | _ -> `Unknown_error)

let users_info token user =
  let%lwt user_id = id_of_user token user in
  endpoint "users.info"
    |> definitely_add "token" token
    |> definitely_add "user" user_id
    |> query (function
    | #authed_result
    | #user_error
    | #user_visibility_error as res -> res
    | _ -> `Unknown_error)

let users_set_active token =
  endpoint "users.setActive"
    |> definitely_add "token" token
    |> query only_auth_can_fail
