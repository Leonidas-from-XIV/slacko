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

(** An OCaml binding to the REST API of Slack. Each function triggers an
    HTTP request, so it returns immediately and returns an Lwt value. *)

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

(** Timestamps are usually represented as floats in OCaml, following suit *)
type timestamp = float

(** Tokens are of a special type, use token_of_string to turn a string into a
    token *)
type token

(** The topic type represents a topic or a purpose message *)
type topic

(** The message represents a message to be posted *)
type message

(** A channel type, can be either a channel name (startingwith a #) or a
    channel id. *)
type channel

(** An user, represented by either a user name or a user id. *)
type user

(** Converts a string into a token *)
val token_of_string: string -> token

val message_of_string: string -> message

val topic_of_string: string -> topic

(** Converts a string into a user. Can be either a username or an user id. *)
val user_of_string: string -> user

(** Converts a string int a channel. Can be either a channel name or a
    channel id. *)
val channel_of_string: string -> user

(** Checks API calling code. *)
val api_test: ?foo:string -> ?error:string -> unit -> [> api_result ] Lwt.t

(** Checks authentication & identity. *)
val auth_test: string -> [> authed_result ] Lwt.t

(** Fetches history of messages and events from a channel. *)
val channels_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> [> history_result ] Lwt.t

(** Gets information about a channel. *)
val channels_info: string -> string -> [> authed_result | channel_error ] Lwt.t

(** Invites a user to a channel. *)
val channels_invite: string -> string -> string -> [> authed_result | channel_error | user_error | invite_error | not_in_channel_error | already_in_channel_error | archive_error ] Lwt.t

(** Joins a channel, creating it if needed. *)
val channels_join: string -> string -> [> authed_result | channel_error | name_error | archive_error ] Lwt.t

(** Removes a user from a channel. *)
val channels_kick: string -> string -> string -> [> authed_result | channel_error | user_error | channel_kick_error | not_in_channel_error | restriction_error ] Lwt.t

(** Leaves a channel. *)
val channels_leave: string -> string -> [> authed_result | channel_error | archive_error | leave_general_error ] Lwt.t

(** Lists all channels in a Slack team. *)
val channels_list: ?exclude_archived:string -> string -> [> authed_result ] Lwt.t

(** Sets the read cursor in a channel. *)
val channels_mark: string -> string -> string -> [> authed_result | channel_error | archive_error | not_in_channel_error ] Lwt.t

(** Sets the purpose for a channel. *)
val channels_set_purpose: string -> string -> string -> [> purpose_result ] Lwt.t

(** Sets the topic for a channel. *)
val channels_set_topic: string -> string -> string -> [> purpose_result ] Lwt.t

(** Deletes a message. *)
val chat_delete: string -> string -> string -> [> authed_result | channel_error | message_error ] Lwt.t

(** Sends a message to a channel. *)
val chat_post_message: string -> string -> ?username:string -> ?parse:string -> ?icon_url:string -> ?icon_emoji:string -> string -> [> authed_result | channel_error | archive_error | message_length_error | rate_error ] Lwt.t

(** Updates a message. *)
val chat_update: string -> string -> string -> string -> [> authed_result | channel_error | message_update_error | message_length_error ] Lwt.t

(** Lists custom emoji for a team. *)
val emoji_list: string -> [> authed_result ] Lwt.t

(** Gets information about a team file. *)
val files_info: string -> ?count:string -> ?page:string -> string -> [> authed_result | file_error ] Lwt.t

(** Lists & filters team files. *)
val files_list: ?user:string -> ?ts_from:string -> ?ts_to:string -> ?types:string -> ?count:string -> ?page:string -> string -> [> authed_result | user_error | unknown_type_error ] Lwt.t

(** Uploads or creates a file. *)
val files_upload: string -> ?filetype:string -> ?filename:string -> ?title:string -> ?initial_comment:string -> ?channels:string -> Cohttp_lwt_body.t -> [> authed_result ] Lwt.t

(** Creates a private group. *)
val groups_create: string -> string -> [> authed_result | name_error | restriction_error ] Lwt.t

(** Clones and archives a private group. *)
val groups_create_child: string -> string -> [> authed_result | channel_error | already_archived_error | restriction_error ] Lwt.t

(** Fetches history of messages and events from a private group. *)
val groups_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> [> history_result ] Lwt.t

(** Invites a user to a private group. *)
val groups_invite: string -> string -> string -> [> authed_result | channel_error | user_error | invite_error | archive_error ] Lwt.t

(** Removes a user from a private group. *)
val groups_kick: string -> string -> string -> [> authed_result | channel_error | user_error | kick_error | not_in_group_error | restriction_error ] Lwt.t

(** Leaves a private group. *)
val groups_leave: string -> string -> [> authed_result | channel_error | archive_error | leave_last_channel_error | last_member_error ] Lwt.t

(** Lists private groups that the calling user has access to. *)
val groups_list: ?exclude_archived:string -> string -> [> authed_result ] Lwt.t

(** Sets the read cursor in a private group. *)
val groups_mark: string -> string -> string -> [> authed_result | channel_error | archive_error | not_in_channel_error ] Lwt.t

(** Sets the purpose for a private group. *)
val groups_set_purpose: string -> string -> string -> [> purpose_result ] Lwt.t

(** Sets the topic for a private group. *)
val groups_set_topic: string -> string -> string -> [> purpose_result ] Lwt.t

(** Fetches history of messages and events from direct message channel. *)
val im_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> [> history_result ] Lwt.t

(** Lists direct message channels for the calling user. *)
val im_list: string -> [> authed_result ] Lwt.t

(** Sets the read cursor in a direct message channel. *)
val im_mark: string -> string -> string -> [> authed_result | channel_error | not_in_channel_error ] Lwt.t

(** Exchanges a temporary OAuth code for an API token. *)
val oauth_access: string -> string -> ?redirect_url:string -> string -> [> api_result | oauth_error ] Lwt.t

(** Manually set user presence *)
val presence_set: string -> string -> [> authed_result | presence_error ] Lwt.t

(** Searches for messages and files matching a query. *)
val search_all: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> [> authed_result ] Lwt.t

(** Searches for files matching a query. *)
val search_files: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> [> authed_result ] Lwt.t

(** Searches for messages matching a query. *)
val search_messages: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> [> authed_result ] Lwt.t

(** Lists stars for a user. *)
val stars_list: ?user:string -> ?count:string -> ?page:string -> string -> [> authed_result | user_error ] Lwt.t

(** Gets information about a user. *)
val users_info: string -> string -> [> authed_result | user_error | user_visibility_error ] Lwt.t

(** Lists all users in a Slack team. *)
val users_list: string -> [> authed_result ] Lwt.t

(** Marks a user as active. *)
val users_set_active: string -> [> authed_result ] Lwt.t
