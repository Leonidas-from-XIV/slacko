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
    HTTP request, so it returns immediately and returns an {!Lwt.t} value.

    To use the API you first need to either apply for a {!token} from Slack,
    or get one via the OAuth2 API. This string can then be converted into
    a {!token} by means of {!token_of_string}. With this {!token} most other
    methods from the binding can be called. The result of each API call is
    a variant type containing either the JSON result or an error type
    describing what kind of error occured.

    @author Marek Kubica *)

(** {2 Types used in the binding} *)

(** The binding exposes a number of errors that can happen. As not every
    function returns every possible API error, the possible errors are grouped
    into more convenient sets of errors that can be composed together to get
    the exact error types.
*)

(** The basic type that each API endpoint will return at least. This type
    includes the case that the request succeeded as well as the case that an
    error happened that the binding didn't know how to handle. It gets
    composed into {e every other} API return type. *)
type api_result = [
  | `Success of Yojson.Basic.json
  | `Unknown_error
]

(** API calls that require authentication (a {!token}) might fail with one of
    these errors, so functions that take {!token} arguments will return
    {e at least} these error variants. *)
type auth_error = [
  | `Not_authed
  | `Invalid_auth
  | `Account_inactive
]

(** API calls that take {!timestamp} arguments can signal errors when the
    timestamp is invalid. The binding does its best to make sure that invalid
    timestamps are avoided. *)
type timestamp_error = [
  | `Invalid_ts_latest
  | `Invalid_ts_oldest
]

(** API calls that require {!channel} inputs can signal this error if the
    channel does not exist. *)
type channel_error = [
  | `Channel_not_found
]

(** API calls that require {!user} inputs signal this error if the user
    was not found. *)
type user_error = [
  | `User_not_found
]

(** Inviting might fail because invitation is impossible for some reason or
    because attempting to invite oneself. *)
type invite_error = [
  | `Cant_invite_self
  | `Cant_invite
]

(** Some API calls require the {!user} to be in {!channel} for the action to
    succeed, not meeting this requirement can raise this error. *)
type not_in_channel_error = [
  | `Not_in_channel
]

(** Some API calls require the {!user} not to be in {!channel} for the action
    to suceed. The opposite of {!not_in_channel_error}. *)
type already_in_channel_error = [
  | `Already_in_channel
]

(** Channels might be archived, so modification attempts will fail with this
    error. *)
type archive_error = [
  | `Is_archived
]

(** When creating channels, names have to be unique, an attempt to create a
    duplicate one will result in this error. *)
type name_error = [
  | `Name_taken
]

(** Kick (in general) might fail, because kicking oneself is not supported. *)
type kick_error = [
  | `Cant_kick_self
]

(** Kicking users from channels might fail, because channels have additional
    restrictions on kicking: users can't be kicked from the #general channel
    and they cannot be kicked from the last channel they are in. *)
type channel_kick_error = [
  | kick_error
  | `Cant_kick_from_general
  | `Cant_kick_from_last_channel
]

(** If an action was attempted that the user does not have permission to,
    this error is returned. *)
type restriction_error = [
  | `Restricted_action
]

(** Leaving the #general channel is not supported by Slack, an attempt do
    do so will trigger this error. *)
type leave_general_error = [
  | `Cant_leave_general
]

(** {!topic} types might be rejected by the Slack API because they are too
    long and can't be set. *)
type topic_error = [
  | `Too_long
]

(** The {!message} might not exist or be impossible to delete for other
    reasons. *)
type message_error = [
  | `Cant_delete_message
  | `Message_not_found
]

(** {!message} types, like {!topic} types might be too long to post. The Slack
    API does not specify the maximum message length, so Slacko can't make sure
    your messages stay below this limit, so everytime you post, this error can
    realistically happen. *)
type message_length_error = [
  | `Msg_too_long
]

(** Doing too many API requests in a certain timespan might cause a rate
    limitation to be applied by Slack. This is the error that results in
    that case. *)
type rate_error = [
  | `Rate_limited
]

(** Updating a {!message} might fail because the message was not found,
    couldn't be updated for some reason or because the time in which a message
    can be edited has passed. *)
type message_update_error = [
  | `Message_not_found
  | `Cant_update_message
  | `Edit_window_closed
]

(** Handling files can result in multiple problems: the file wasn't found in
    the first place or it might have been deleted in the maintime. *)
type file_error = [
  | `File_not_found
  | `File_deleted
]

(** This error shouldn't ever be returned but serves as a catch-all in case
    the Slack API returns a new, unknown error type that Slacko doesn't yet
    understand. *)
type unknown_type_error = [
  | `Unknown_type
]

(** When trying to archive something that was already archived before, this
    error is returned. *)
type already_archived_error = [
  | `Already_archived
]

(** Doing an action in a {!group} when not being part of the group can fail. *)
type not_in_group_error = [
  | `Not_in_group
]

(* Leaving the last {!channel} is not supported by Slack. *)
type leave_last_channel_error = [
  | `Cant_leave_last_channel
]

(** An error when the user is the last member and can't do what he planned to
    do because that would cause the {!channel} not to have members anymore. *)
type last_member_error = [
  | `Last_member
]

(** These errors might be returned when the exchange of oauth authorization
    for a token has failed. *)
type oauth_error = [
  | `Invalid_client_id
  | `Bad_client_secret
  | `Invalid_code
  | `Bad_redirect_uri
]

(** Setting an invalid presence information is not supported. *)
type presence_error = [
  | `Invalid_presence
]

(** User is not visible, so action cannot be performed on them. *)
type user_visibility_error = [
  | `User_not_visible
]

type invalid_name_error = [
  | `Invalid_name
]

(** API calls which require authentication will always return (at least) these
    error types. *)
type authed_result = [
  | api_result
  | auth_error
]

(** Setting topics or purposes will result either in a success or one of these
    errors. Convenience type composed of subtypes. *)
type topic_result = [
  | authed_result
  | channel_error
  | archive_error
  | not_in_channel_error
  | topic_error
]

(** Return value of a history related request. *)
type history_result = [
  | authed_result
  | channel_error
  | timestamp_error
]

(** In Slack, timestamps are represented as float, same as in OCaml, so the
    {!timestamp} type is following suit, so all timestamp-related functions
    can be used. *)
type timestamp = float

(** Tokens are required in the API for all actions that require
    authentication. *)
type token

(** The topic type represents a topic or a purpose message. Both are limited
    deliberately to have at most 250 UTF-8 encoded codepoints. *)
type topic

(** The message represents a message to be posted. *)
type message

(** A channel type, can be either a channel name (starting with a #) or a
    channel id. *)
type channel

(** An user, represented by either a user name or a user id. *)
type user

(** A group, a private subset of users chatting together. *)
type group

(** What criterion to use in search. *)
type sort_criterion = Score | Timestamp

(** Search result can be ordered in ascending or descending order. *)
type sort_direction = Ascending | Descending

(** Presence can either be active or away. *)
type presence = Active | Away

(** {2 Type construction helper functions} *)

(** To build the types required in the API calls, you can use these helper
    functions. *)

(** Converts a string into a token. *)
val token_of_string: string -> token

(** Build a message from a string. *)
val message_of_string: string -> message

(** Build a topic out of a string. {!topic} types are also used to
    set purposes. Also validates the length of the topic, since Slack has
    a 250 UTF-8 codepoint length limit on purposes and topics. *)
val topic_of_string: string -> topic option

(** Same as {!topic_of_string} but throws an exception if it fails to convert
    the text data into a {!topic}. *)
val topic_of_string_exn: string -> topic

(** Construct a group out of a given string. This can be either a group id,
    starting with capital 'G' character which is the preferred way or it can
    be a group name for convenience. In the latter case, each API call with
    requires a group will perform an additional request to determine the group
    id from the name. *)
val group_of_string: string -> group

(** Constructs a user out of a given string. The string can either be an user
    id starting with a capital 'U' which is the preferred way or it can be a
    simple user name in which case every API call will look up the user name
    to an id in an additional request. *)
val user_of_string: string -> user

(** Constructs a channel out of a given string. Can either be a channel id
    starting with a capital 'C' which is the preferred way or a channel name
    starting with a '#'. If a channel name was provided, each consecutive API
    call using it will first need to resolve the channel name into a channel
    id by means of an additional request. *)
val channel_of_string: string -> channel

(** {2 Slack API calls} *)

(** Checks API calling code.
    @param foo A dummy value that will be returned by the API.
    @param error If set, will return a specific kind of error. *)
val api_test: ?foo:string -> ?error:string -> unit -> [> api_result ] Lwt.t

(** Checks authentication & identity.
    @param token The authentication token that was issued by Slack. *)
val auth_test: token -> [> authed_result ] Lwt.t

(** Fetches history of messages and events from a channel.
    @param token The authentication token that was issued by Slack.
    @param latest The newest message from history to be returned.
    @param oldest The oldest message from history to be returned.
    @param count The number of messages to be returned.
    @param channel The Slack channel from which to get the history. *)
val channels_history: token -> ?latest:timestamp -> ?oldest:timestamp -> ?count:int -> channel -> [> history_result ] Lwt.t

(** Gets information about a channel. *)
val channels_info: token -> channel -> [> authed_result | channel_error ] Lwt.t

(** Invites a user to a channel. *)
val channels_invite: token -> channel -> user -> [> authed_result | channel_error | user_error | invite_error | not_in_channel_error | already_in_channel_error | archive_error ] Lwt.t

(** Joins a channel, creating it if needed. *)
val channels_join: token -> channel -> [> authed_result | channel_error | name_error | archive_error ] Lwt.t

(** Removes a user from a channel. *)
val channels_kick: token -> channel -> user -> [> authed_result | channel_error | user_error | channel_kick_error | not_in_channel_error | restriction_error ] Lwt.t

(** Leaves a channel. *)
val channels_leave: token -> channel -> [> authed_result | channel_error | archive_error | leave_general_error ] Lwt.t

(** Lists all channels in a Slack team. *)
val channels_list: ?exclude_archived:bool -> token -> [> authed_result ] Lwt.t

(** Sets the read cursor in a channel. *)
val channels_mark: token -> channel -> timestamp -> [> authed_result | channel_error | archive_error | not_in_channel_error ] Lwt.t

(** Renames a team channel. *)
val channels_rename: token -> channel -> string -> [> authed_result | channel_error | not_in_channel_error | name_error | invalid_name_error | `Not_authorized | `User_is_restricted ] Lwt.t

(** Sets the purpose for a channel. *)
val channels_set_purpose: token -> channel -> topic -> [> topic_result ] Lwt.t

(** Sets the topic for a channel. *)
val channels_set_topic: token -> channel -> topic -> [> topic_result ] Lwt.t

(** Deletes a message. *)
val chat_delete: token -> timestamp -> channel -> [> authed_result | channel_error | message_error ] Lwt.t

(** Sends a message to a channel. *)
val chat_post_message: token -> channel -> ?username:string -> ?parse:string -> ?icon_url:string -> ?icon_emoji:string -> message -> [> authed_result | channel_error | archive_error | message_length_error | rate_error ] Lwt.t

(** Updates a message. *)
val chat_update: token -> timestamp -> channel -> message -> [> authed_result | channel_error | message_update_error | message_length_error ] Lwt.t

(** Lists custom emoji for a team. *)
val emoji_list: token -> [> authed_result ] Lwt.t

(** Gets information about a team file. *)
val files_info: token -> ?count:int -> ?page:int -> string -> [> authed_result | file_error ] Lwt.t

(** Lists & filters team files. *)
val files_list: ?user:user -> ?ts_from:timestamp -> ?ts_to:timestamp -> ?types:string -> ?count:int -> ?page:int -> token -> [> authed_result | user_error | unknown_type_error ] Lwt.t

(** Uploads or creates a file. *)
val files_upload: token -> ?filetype:string -> ?filename:string -> ?title:string -> ?initial_comment:string -> ?channels:string -> Cohttp_lwt_body.t -> [> authed_result ] Lwt.t

(** Creates a private group. *)
val groups_create: token -> group -> [> authed_result | name_error | restriction_error ] Lwt.t

(** Clones and archives a private group. *)
val groups_create_child: token -> group -> [> authed_result | channel_error | already_archived_error | restriction_error ] Lwt.t

(** Fetches history of messages and events from a private group. *)
val groups_history: token -> ?latest:timestamp -> ?oldest:timestamp -> ?count:int -> group -> [> history_result ] Lwt.t

(** Invites a user to a private group. *)
val groups_invite: token -> group -> user -> [> authed_result | channel_error | user_error | invite_error | archive_error ] Lwt.t

(** Removes a user from a private group. *)
val groups_kick: token -> group -> user -> [> authed_result | channel_error | user_error | kick_error | not_in_group_error | restriction_error ] Lwt.t

(** Leaves a private group. *)
val groups_leave: token -> group -> [> authed_result | channel_error | archive_error | leave_last_channel_error | last_member_error ] Lwt.t

(** Lists private groups that the calling user has access to. *)
val groups_list: ?exclude_archived:bool -> token -> [> authed_result ] Lwt.t

(** Sets the read cursor in a private group. *)
val groups_mark: token -> group -> timestamp -> [> authed_result | channel_error | archive_error | not_in_channel_error ] Lwt.t

(** Renames a private group. *)
val groups_rename: token -> group -> string -> [> authed_result | channel_error | name_error | invalid_name_error | `User_is_restricted ] Lwt.t

(** Sets the purpose for a private group. *)
val groups_set_purpose: token -> group -> topic -> [> topic_result ] Lwt.t

(** Sets the topic for a private group. *)
val groups_set_topic: token -> group -> topic -> [> topic_result ] Lwt.t

(** Fetches history of messages and events from direct message channel. *)
val im_history: token -> ?latest:timestamp -> ?oldest:timestamp -> ?count:int -> string -> [> history_result ] Lwt.t

(** Lists direct message channels for the calling user. *)
val im_list: token -> [> authed_result ] Lwt.t

(** Sets the read cursor in a direct message channel. *)
val im_mark: token -> string -> timestamp -> [> authed_result | channel_error | not_in_channel_error ] Lwt.t

(** Exchanges a temporary OAuth code for an API token. *)
val oauth_access: string -> string -> ?redirect_url:string -> string -> [> api_result | oauth_error ] Lwt.t

(** Manually set user presence. *)
val presence_set: token -> presence -> [> authed_result | presence_error ] Lwt.t

(** Searches for messages and files matching a query. *)
val search_all: token -> ?sort:sort_criterion -> ?sort_dir:sort_direction -> ?highlight:bool -> ?count:int -> ?page:int -> string -> [> authed_result ] Lwt.t

(** Searches for files matching a query. *)
val search_files: token -> ?sort:sort_criterion -> ?sort_dir:sort_direction -> ?highlight:bool -> ?count:int -> ?page:int -> string -> [> authed_result ] Lwt.t

(** Searches for messages matching a query. *)
val search_messages: token -> ?sort:sort_criterion -> ?sort_dir:sort_direction -> ?highlight:bool -> ?count:int -> ?page:int -> string -> [> authed_result ] Lwt.t

(** Lists stars for a user. *)
val stars_list: ?user:user -> ?count:int -> ?page:int -> token -> [> authed_result | user_error ] Lwt.t

(** Gets information about a user. *)
val users_info: token -> user -> [> authed_result | user_error | user_visibility_error ] Lwt.t

(** Lists all users in a Slack team. *)
val users_list: token -> [> authed_result ] Lwt.t

(** Marks a user as active. *)
val users_set_active: token -> [> authed_result ] Lwt.t
