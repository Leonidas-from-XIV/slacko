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

(** Slack API *)

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

val api_test: ?foo:string -> ?error:string -> unit -> apierror Lwt.t
val auth_test: string -> apierror Lwt.t
val channels_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> apierror Lwt.t
val channels_info: string -> string -> apierror Lwt.t
val channels_invite: string -> string -> string -> apierror Lwt.t
val channels_join: string -> string -> apierror Lwt.t
val channels_kick: string -> string -> string -> apierror Lwt.t
val channels_leave: string -> string -> apierror Lwt.t
val channels_list: ?exclude_archived:string -> string -> apierror Lwt.t
val channels_mark: string -> string -> string -> apierror Lwt.t
val channels_set_purpose: string -> string -> string -> apierror Lwt.t
val channels_set_topic: string -> string -> string -> apierror Lwt.t
val chat_delete: string -> string -> string -> apierror Lwt.t

(** Posts message *)
val chat_post_message: string -> string -> ?username:string -> ?parse:string -> ?icon_url:string -> ?icon_emoji:string -> string -> apierror Lwt.t
val chat_update: string -> string -> string -> string -> apierror Lwt.t
val emoji_list: string -> apierror Lwt.t
val files_info: string -> ?count:string -> ?page:string -> string -> apierror Lwt.t
val files_list: ?user:string -> ?ts_from:string -> ?ts_to:string -> ?types:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t
val files_upload: string -> ?filetype:string -> ?filename:string -> ?title:string -> ?initial_comment:string -> ?channels:string -> Cohttp_lwt_body.t -> apierror Lwt.t
val groups_create: string -> string -> apierror Lwt.t
val groups_create_child: string -> string -> apierror Lwt.t
val groups_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> apierror Lwt.t
val groups_invite: string -> string -> string -> apierror Lwt.t
val groups_kick: string -> string -> string -> apierror Lwt.t
val groups_leave: string -> string -> apierror Lwt.t
val groups_list: ?exclude_archived:string -> string -> apierror Lwt.t
val groups_mark: string -> string -> string -> apierror Lwt.t
val groups_set_purpose: string -> string -> string -> apierror Lwt.t
val groups_set_topic: string -> string -> string -> apierror Lwt.t
val im_history: string -> ?latest:string -> ?oldest:string -> ?count:string -> string -> apierror Lwt.t
val im_list: string -> apierror Lwt.t
val im_mark: string -> string -> string -> apierror Lwt.t
val oauth_access: string -> string -> ?redirect_url:string -> string -> apierror Lwt.t
val presence_set: string -> string -> apierror Lwt.t
val search_all: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t
val search_files: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t
val search_messages: string -> ?sort:string -> ?sort_dir:string -> ?highlight:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t
val stars_list: ?user:string -> ?count:string -> ?page:string -> string -> apierror Lwt.t
val users_list: string -> apierror Lwt.t
val users_set_active: string -> apierror Lwt.t
