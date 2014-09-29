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

type apierror =
           [ `Account_inactive
            | `Already_in_channel
            | `Bad_client_secret
            | `Bad_redirect_uri
            | `Cant_delete_message
            | `Cant_invite
            | `Cant_invite_self
            | `Cant_kick_from_general
            | `Cant_kick_from_last_channel
            | `Cant_kick_self
            | `Cant_leave_general
            | `Cant_leave_last_channel
            | `Cant_update_message
            | `Channel_not_found
            | `Edit_window_closed
            | `Error
            | `File_deleted
            | `File_not_found
            | `Invalid_auth
            | `Invalid_client_id
            | `Invalid_code
            | `Invalid_presence
            | `Invalid_ts_latest
            | `Invalid_ts_oldest
            | `Is_archived
            | `Last_member
            | `Message_not_found
            | `Msg_too_long
            | `Name_taken
            | `No_channel
            | `No_text
            | `Not_authed
            | `Not_in_channels
            | `Rate_limited
            | `Restricted_action
            | `Success of Yojson.Basic.json
            | `Too_long
            | `Unknown_type
            | `User_not_found ]

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
      | `String "message_not_found" -> `Message_not_found
      | `String "cant_delete_message" -> `Cant_delete_message
      | `String "cant_update_message" -> `Cant_update_message
      | `String "edit_window_closed" -> `Edit_window_closed
      | `String "no_text" -> `No_text
      | `String "file_not_found" -> `File_not_found
      | `String "file_deleted" -> `File_deleted
      | `String "unknown_type" -> `Unknown_type
      | `String "cant_leave_last_channel" -> `Cant_leave_last_channel
      | `String "last_member" -> `Last_member
      | `String "invalid_client_id" -> `Invalid_client_id
      | `String "bad_client_secret" -> `Bad_client_secret
      | `String "invalid_code" -> `Invalid_code
      | `String "bad_redirect_uri" -> `Bad_redirect_uri
      | `String "invalid_presence" -> `Invalid_presence
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

(* do a POST request *)
let query_post uri body =
  lwt (_, body) = Cohttp_unix.Client.post ~body uri in
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

let channels_set_topic token channel topic =
  let uri = endpoint "channels.setTopic"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "topic" topic
  in query uri

let chat_delete token ts channel =
  let uri = endpoint "chat.delete"
    |> definitely_add "token" token
    |> definitely_add "ts" ts
    |> definitely_add "channel" channel
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

let chat_update token ts channel text =
  let uri = endpoint "chat.update"
    |> definitely_add "token" token
    |> definitely_add "ts" ts
    |> definitely_add "channel" channel
    |> definitely_add "text" text
  in query uri

let emoji_list token =
  let uri = endpoint "emoji.list"
    |> definitely_add "token" token
  in query uri

let files_info token ?count ?page file =
  let uri = endpoint "files.info"
    |> definitely_add "token" token
    |> definitely_add "file" file
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri

let files_list ?user ?ts_from ?ts_to ?types ?count ?page token =
  let uri = endpoint "files.list"
    |> definitely_add "token" token
    |> optionally_add "user" user
    |> optionally_add "ts_from" ts_from
    |> optionally_add "ts_to" ts_to
    |> optionally_add "types" types
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri

let files_upload token
  ?filetype ?filename ?title ?initial_comment ?channels content =
  let uri = endpoint "files.upload"
    |> definitely_add "token" token
    |> optionally_add "filetype" filetype
    |> optionally_add "filename" filename
    |> optionally_add "title" title
    |> optionally_add "initial_comment" initial_comment
    |> optionally_add "channels" channels
  in query_post uri

let groups_create token name =
  let uri = endpoint "groups.create"
    |> definitely_add "token" token
    |> definitely_add "name" name
  in query uri

let groups_create_child token channel =
  let uri = endpoint "groups.createChild"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri

let groups_history token ?latest ?oldest ?count channel =
  let uri = endpoint "groups.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" latest
    |> optionally_add "oldest" oldest
    |> optionally_add "count" count
  in query uri

let groups_invite token channel user =
  let uri = endpoint "groups.invite"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri

let groups_kick token channel user =
  let uri = endpoint "groups.kick"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "user" user
  in query uri

let groups_leave token channel =
  let uri = endpoint "groups.leave"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
  in query uri

let groups_list ?exclude_archived token =
  let uri = endpoint "groups.list"
    |> definitely_add "token" token
    |> optionally_add "exclude_archived" exclude_archived
  in query uri

let groups_mark token channel ts =
  let uri = endpoint "groups.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" ts
  in query uri

let groups_set_purpose token channel purpose =
  let uri = endpoint "groups.setPurpose"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "purpose" purpose
  in query uri

let groups_set_topic token channel topic =
  let uri = endpoint "groups.setTopic"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "topic" topic
  in query uri

let im_history token ?latest ?oldest ?count channel =
  let uri = endpoint "im.history"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> optionally_add "latest" latest
    |> optionally_add "oldest" oldest
    |> optionally_add "count" count
  in query uri

let im_list token =
  let uri = endpoint "im.list"
    |> definitely_add "token" token
  in query uri

let im_mark token channel ts =
  let uri = endpoint "im.mark"
    |> definitely_add "token" token
    |> definitely_add "channel" channel
    |> definitely_add "ts" ts
  in query uri

let oauth_access client_id client_secret ?redirect_url code =
  let uri = endpoint "oauth.access"
    |> definitely_add "client_id" client_id
    |> definitely_add "client_secret" client_secret
    |> definitely_add "code" code
    |> optionally_add "redirect_url" redirect_url
  in query uri

let presence_set token presence =
  let uri = endpoint "presence.set"
    |> definitely_add "token" token
    |> definitely_add "presence" presence
  in query uri

let search_all token ?sort ?sort_dir ?highlight ?count ?page query_ =
  let uri = endpoint "search.all"
    |> definitely_add "token" token
    |> definitely_add "query" query_
    |> optionally_add "sort" sort
    |> optionally_add "sort_dir" sort_dir
    |> optionally_add "highlight" highlight
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri

let search_files token ?sort ?sort_dir ?highlight ?count ?page query_ =
  let uri = endpoint "search.files"
    |> definitely_add "token" token
    |> definitely_add "query" query_
    |> optionally_add "sort" sort
    |> optionally_add "sort_dir" sort_dir
    |> optionally_add "highlight" highlight
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri

let search_messages token ?sort ?sort_dir ?highlight ?count ?page query_ =
  let uri = endpoint "search.files"
    |> definitely_add "token" token
    |> definitely_add "query" query_
    |> optionally_add "sort" sort
    |> optionally_add "sort_dir" sort_dir
    |> optionally_add "highlight" highlight
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri

let stars_list ?user ?count ?page token =
  let uri = endpoint "stars.list"
    |> definitely_add "token" token
    |> optionally_add "user" user
    |> optionally_add "count" count
    |> optionally_add "page" page
  in query uri

let users_list token =
  let uri = endpoint "users.list"
    |> definitely_add "token" token
  in query uri

let users_set_active token =
  let uri = endpoint "users.setActive"
    |> definitely_add "token" token
  in query uri
