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
            | `Not_in_channel
            | `Rate_limited
            | `Restricted_action
            | `Success of Yojson.Basic.json
            | `Too_long
            | `Unknown_type
            | `User_not_found
            | `User_not_visible ]

(** Checks API calling code. *)
val api_test: ?foo:string -> ?error:string -> unit -> apierror Lwt.t

(** Checks authentication & identity. *)
val auth_test: string -> apierror Lwt.t

(** Fetches history of messages and events from a channel. *)
val channels_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> apierror Lwt.t

(** Gets information about a channel. *)
val channels_info: string -> string -> apierror Lwt.t

(** Invites a user to a channel. *)
val channels_invite: string -> string -> string -> apierror Lwt.t

(** Joins a channel, creating it if needed. *)
val channels_join: string -> string -> apierror Lwt.t

(** Removes a user from a channel. *)
val channels_kick: string -> string -> string -> apierror Lwt.t

(** Leaves a channel. *)
val channels_leave: string -> string -> apierror Lwt.t

(** Lists all channels in a Slack team. *)
val channels_list: ?exclude_archived:string -> string -> apierror Lwt.t

(** Sets the read cursor in a channel. *)
val channels_mark: string -> string -> string -> apierror Lwt.t

(** Sets the purpose for a channel. *)
val channels_set_purpose: string -> string -> string -> apierror Lwt.t

(** Sets the topic for a channel. *)
val channels_set_topic: string -> string -> string -> apierror Lwt.t

(** Deletes a message. *)
val chat_delete: string -> string -> string -> apierror Lwt.t

(** Sends a message to a channel. *)
val chat_post_message: string -> string -> ?username:string -> ?parse:string -> ?icon_url:string -> ?icon_emoji:string -> string -> apierror Lwt.t

(** Updates a message. *)
val chat_update: string -> string -> string -> string -> apierror Lwt.t

(** Lists custom emoji for a team. *)
val emoji_list: string -> apierror Lwt.t

(** Gets information about a team file. *)
val files_info: string -> ?count:string -> ?page:string -> string -> apierror Lwt.t

(** Lists & filters team files. *)
val files_list: ?user:string -> ?ts_from:string -> ?ts_to:string -> ?types:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t

(** Uploads or creates a file. *)
val files_upload: string -> ?filetype:string -> ?filename:string -> ?title:string -> ?initial_comment:string -> ?channels:string -> Cohttp_lwt_body.t -> apierror Lwt.t

(** Creates a private group. *)
val groups_create: string -> string -> apierror Lwt.t

(** Clones and archives a private group. *)
val groups_create_child: string -> string -> apierror Lwt.t

(** Fetches history of messages and events from a private group. *)
val groups_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> apierror Lwt.t

(** Invites a user to a private group. *)
val groups_invite: string -> string -> string -> apierror Lwt.t

(** Removes a user from a private group. *)
val groups_kick: string -> string -> string -> apierror Lwt.t

(** Leaves a private group. *)
val groups_leave: string -> string -> apierror Lwt.t

(** Lists private groups that the calling user has access to. *)
val groups_list: ?exclude_archived:string -> string -> apierror Lwt.t

(** Sets the read cursor in a private group. *)
val groups_mark: string -> string -> string -> apierror Lwt.t

(** Sets the purpose for a private group. *)
val groups_set_purpose: string -> string -> string -> apierror Lwt.t

(** Sets the topic for a private group. *)
val groups_set_topic: string -> string -> string -> apierror Lwt.t

(** Fetches history of messages and events from direct message channel. *)
val im_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> apierror Lwt.t

(** Lists direct message channels for the calling user. *)
val im_list: string -> apierror Lwt.t

(** Sets the read cursor in a direct message channel. *)
val im_mark: string -> string -> string -> apierror Lwt.t

(** Exchanges a temporary OAuth code for an API token. *)
val oauth_access: string -> string -> ?redirect_url:string -> string -> apierror Lwt.t

(** Manually set user presence *)
val presence_set: string -> string -> apierror Lwt.t

(** Searches for messages and files matching a query. *)
val search_all: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t

(** Searches for files matching a query. *)
val search_files: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t

(** Searches for messages matching a query. *)
val search_messages: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t

(** Lists stars for a user. *)
val stars_list: ?user:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t

(** Gets information about a user. *)
val users_info: string -> string -> apierror Lwt.t

(** Lists all users in a Slack team. *)
val users_list: string -> apierror Lwt.t

(** Marks a user as active. *)
val users_set_active: string -> apierror Lwt.t
