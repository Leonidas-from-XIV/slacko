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


(** Posts message *)
val chat_post_message: string -> string -> ?username:string -> ?parse:string -> ?icon_url:string -> ?icon_emoji:string -> string -> apierror Lwt.t
