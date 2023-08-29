(*
* Slacko - Binding to the Slack API
* Copyright (C) 2014-2016 Marek Kubica <marek@xivilization.net>
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

open Lwt.Infix
open Lwt.Syntax
module Cohttp_unix = Cohttp_lwt_unix
module Cohttp_body = Cohttp_lwt.Body

type api_error = [
  | `Unhandled_error of string
  | `Unknown_error
]

type parsed_api_error = [
  | `ParseFailure of string
  | api_error
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

type message_error = [
  | `Cant_delete_message
  | `Message_not_found
]

type message_length_error = [
  | `Msg_too_long
]

type attachments_error = [
  | `Too_many_attachments
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
  | `Unknown_error
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

type bot_error = [
  | `User_is_bot
]

type parsed_auth_error = [
  | parsed_api_error
  | auth_error
]

type topic_result = [
  | `Success of string
  | parsed_auth_error
  | channel_error
  | archive_error
  | not_in_channel_error
  | `User_is_restricted
]

type timestamp = Ptime.t

type session = {
  base_url: string;
  token: string;
  (* Mutable id cache goes here? *)
}

type token = session

type topic = string

type message = string

type channel = ChannelId of string | ChannelName of string

type im = string

type user = UserId of string | UserName of string

type bot = BotId of string

type group = GroupId of string | GroupName of string

(* TODO: Sure about user? *)
type chat = Channel of channel | Im of im | User of user | Group of group

type sort_criterion = Score | Timestamp

type sort_direction = Ascending | Descending

type presence = Auto | Away

let user_of_yojson = function
  | `String x -> Ok (UserId x)
  | _ -> Error "Couldn't parse user type"

let bot_of_string s =
  if s.[0] = 'B' then BotId s else invalid_arg ("bot_of_string " ^ s)

let bot_of_yojson = function
  | `String x -> Ok (bot_of_string x)
  | _ -> Error "Couldn't parse bot type"


let channel_of_yojson = function
  | `String x -> Ok (ChannelId x)
  | _ -> Error "Couldn't parse channel type"

let group_of_yojson = function
  | `String x -> Ok (GroupId x)
  | _ -> Error "Couldn't parse group type"

let im_of_yojson = function
  | `String x -> Ok x
  | _ -> Error "Couldn't parse im type"

type topic_obj = {
  value: string;
  creator: user;
  last_set: Timestamp.t;
} [@@deriving of_yojson]

type channel_obj = {
  id: channel;
  name: string;
  is_channel: bool;
  created: Timestamp.t;
  creator: user;
  is_archived: bool;
  is_general: bool;
  is_member: bool;
  members: user list;
  topic: topic_obj;
  purpose: topic_obj;
  last_read: Timestamp.t option [@default None];
  latest: Yojson.Safe.t option [@default None];
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
  num_members: int option [@default None];
} [@@deriving of_yojson { strict = false }]

type user_obj = {
  id: user;
  name: string;
  deleted: bool;
  color: string option [@default None];
  real_name: string option [@default None];
  tz: string option [@default None];
  tz_label: string option [@default None];
  tz_offset: int [@default 0];
  profile: Yojson.Safe.t;
  is_admin: bool [@default false];
  is_owner: bool [@default false];
  is_primary_owner: bool [@default false];
  is_restricted: bool [@default false];
  is_ultra_restricted: bool [@default false];
  is_bot: bool;
  has_files: bool [@default false];
} [@@deriving of_yojson { strict = false } ]

type group_obj = {
  id: group;
  name: string;
  is_group: bool;
  created: Timestamp.t;
  creator: user;
  is_archived: bool;
  members: user list;
  topic: topic_obj;
  purpose: topic_obj;
  is_open: bool option [@default None];
  last_read: Timestamp.t option [@default None];
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
  latest: Yojson.Safe.t option [@default None];
} [@@deriving of_yojson { strict = false }]

type file_obj = {
  (* TODO file id type *)
  id: string;
  created: Timestamp.t;
  (* deprecated *)
  timestamp: Timestamp.t;

  name: string option [@default None];
  title: string;
  mimetype: string;
  pretty_type: string;
  user: user;

  mode: string;
  editable: bool;
  is_external: bool;
  external_type: string;

  size: int;

  url_private: string;
  url_private_download: string;

  thumb_64: string option [@default None];
  thunb_80: string option [@default None];
  thumb_360: string option [@default None];
  thumb_360_gif: string option [@default None];
  thumb_360_w: int option [@default None];
  thumb_360_h: int option [@default None];

  permalink: string;
  edit_link: string option [@default None];
  preview: string option [@default None];
  preview_highlight: string option [@default None];
  lines: int option [@default None];
  lines_more: int option [@default None];

  is_public: bool;
  (*public_url_shared: ???;*)
  channels: channel list;
  groups: group list;
  ims: im list;
  initial_comment: Yojson.Safe.t option [@default None];
  num_stars: int option [@default None];
} [@@deriving of_yojson { strict = false }]

type field_obj = {
  title: string option [@default None];
  value: string [@default ""];
  short: bool [@default false];
} [@@deriving to_yojson { strict = false }]

let field ?title ?(short=false) value = {
  title;
  value;
  short;
}

type attachment_obj = {
  fallback: string option [@default None];
  color: string option [@default None];
  pretext: string option [@default None];
  author_name: string option [@default None];
  author_link: string option [@default None];
  author_icon: string option [@default None];
  title: string option [@default None];
  title_link: string option [@default None];
  text: string option [@default None];
  fields: field_obj list option [@default None];
  image_url: string option [@default None];
  thumb_url: string option [@default None];
  footer: string option [@default None];
  footer_icon: string option [@default None];
  ts: Timestamp.t option [@default None];
  mrkdwn_in: string list option [@default None];
} [@@deriving to_yojson { strict = false }]

let if_none a b =
  match a with
  | Some v -> Some v
  | None -> b

let attachment
    ?fallback ?color ?pretext
    ?author_name ?author_link ?author_icon
    ?title ?title_link
    ?text ?fields
    ?image_url ?thumb_url
    ?footer ?footer_icon
    ?ts ?mrkdwn_in
    () = {
  fallback = if_none fallback text;
  color;
  pretext;
  author_name;
  author_link;
  author_icon;
  title;
  title_link;
  text;
  fields;
  image_url;
  thumb_url;
  footer;
  footer_icon;
  ts;
  mrkdwn_in;
}

type message_obj = {
  type': string [@key "type"];
  ts: Timestamp.t;
  user: user option [@default None];
  bot_id: bot option [@default None];
  text: string option;
  is_starred: bool option [@default None];
} [@@deriving of_yojson { strict = false }]

type history_obj = {
  latest: Timestamp.t option [@default None];
  messages: message_obj list;
  has_more: bool;
} [@@deriving of_yojson { strict = false }]

type authed_obj = {
  url: string;
  team: string;
  user: string;
  team_id: string;
  user_id: user;
} [@@deriving of_yojson { strict = false }]

type channel_leave_obj = {
  not_in_channel: bool option [@default None];
} [@@deriving of_yojson { strict = false }]

type channel_rename_obj = {
  id: channel;
  is_channel: bool;
  name: string;
  created: Timestamp.t;
} [@@deriving of_yojson { strict = false }]

let chat_of_yojson = function
  | `String c -> (match c.[0] with
    | 'C' -> Ok (Channel (ChannelId c))
    | 'D' -> Ok (Im c)
    | 'G' -> Ok (Group (GroupId c))
    | _ -> Error "Failed to parse chat")
  | _ -> Error "Failed to parse chat"

type chat_obj = {
  ts: Timestamp.t;
  chat: chat [@key "channel"];
  text: string option [@default None];
} [@@deriving of_yojson { strict = false }]

type emoji = (string * string)
type emoji_list_obj = {
  emoji: (string * string) list;
} [@@deriving of_yojson]

type chat_close_obj = {
  no_op: bool option [@default None];
  already_closed: bool option [@default None];
} [@@deriving of_yojson { strict = false }]

type groups_invite_obj = {
  already_in_group: bool option [@default None];
  group: group_obj;
} [@@deriving of_yojson { strict = false }]

type groups_open_obj = {
  no_op: bool option [@default None];
  already_open: bool option [@default None];
} [@@deriving of_yojson { strict = false }]

type groups_rename_obj = {
  id: channel;
  is_group: bool;
  name: string;
  created: Timestamp.t
} [@@deriving of_yojson { strict = false }]

type im_obj = {
  id: string;
  is_im: bool;
  user: user;
  created: Timestamp.t;
  is_user_deleted: bool;
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
} [@@deriving of_yojson { strict = false }]

type im_channel_obj = {
  id: string;
} [@@deriving of_yojson { strict = false }]

type im_open_obj = {
  no_op: bool option [@default None];
  already_open: bool option [@default None];
  channel: im_channel_obj;
} [@@deriving of_yojson { strict = false }]

type oauth_obj = {
  access_token: string;
  scope: string;
} [@@deriving of_yojson { strict = false }]

type comment_obj = {
  id: string;
  timestamp: Timestamp.t;
  user: user;
  comment: string;
} [@@deriving of_yojson { strict = false }]

type paging_obj = {
  count: int;
  total: int;
  page: int;
  pages: int;
} [@@deriving of_yojson { strict = false }]

type files_info_obj = {
  file: file_obj;
  comments: comment_obj list;
  paging: paging_obj;
} [@@deriving of_yojson { strict = false }]

type files_list_obj = {
  files: file_obj list;
  paging: paging_obj;
} [@@deriving of_yojson { strict = false }]

type stars_list_obj = {
  (* TODO proper types *)
  items: Yojson.Safe.t list;
  paging: paging_obj;
} [@@deriving of_yojson { strict = false }]

type message_search_obj = {
  total: int;
  paging: paging_obj;
  matches: message_obj list;
} [@@deriving of_yojson { strict = false }]

type file_search_obj = {
  total: int;
  paging: paging_obj;
  matches: file_obj list;
} [@@deriving of_yojson { strict = false }]

type search_obj = {
  query: string;
  messages: message_search_obj option [@default None];
  files: file_search_obj option [@default None];
} [@@deriving of_yojson { strict = false }]

type team_obj = {
  (* TODO team id *)
  id: string;
  name: string;
  domain: string;
  email_domain: string;
  icon: Yojson.Safe.t;
} [@@deriving of_yojson { strict = false }]

type login_obj = {
  user_id: user;
  username: string;
  date_first: Timestamp.t;
  date_last: Timestamp.t;
  count: int;
  ip: string;
  user_agent: string;
  isp: string;
  country: string;
  region: string;
} [@@deriving of_yojson { strict = false }]

type team_access_log_obj = {
  logins: login_obj list;
  paging: paging_obj;
} [@@deriving of_yojson { strict = false }]

type history_result = [
  | `Success of history_obj
  | parsed_auth_error
  | channel_error
  | timestamp_error
]

(* internal *)
let default_base_url = "https://slack.com/api/"

type api_request = {
  method': string;
  arguments: (string * string) list
}

let api_request method' = { method'; arguments = [] }

let optionally_add key value request = match value with
  | None -> request
  | Some value -> { request with arguments = (key, value)::request.arguments }

let definitely_add key value = optionally_add key (Some value)

(* private API return type *)
(* the strict is important here, because we just match ok & error and
 * deliberately ignore the rest *)
type api_answer = {
  ok: bool;
  error: string option [@default None]
} [@@deriving of_yojson { strict = false }]

let validate json =
  match api_answer_of_yojson json with
  | Error str -> `ParseFailure str
  | Ok parsed -> match parsed.ok, parsed.error with
    | true, _ -> `Json_response json
    | _, Some "account_inactive" -> `Account_inactive
    | _, Some "already_archived" -> `Already_archived
    | _, Some "already_in_channel" -> `Already_in_channel
    | _, Some "bad_client_secret" -> `Bad_client_secret
    | _, Some "bad_redirect_uri" -> `Bad_redirect_uri
    | _, Some "cant_archive_general" -> `Cant_archive_general
    | _, Some "cant_invite" -> `Cant_invite
    | _, Some "cant_invite_self" -> `Cant_invite_self
    | _, Some "cant_delete_file" -> `Cant_delete_file
    | _, Some "cant_delete_message" -> `Cant_delete_message
    | _, Some "cant_kick_from_general" -> `Cant_kick_from_general
    | _, Some "cant_kick_from_last_channel" -> `Cant_kick_from_last_channel
    | _, Some "cant_kick_self" -> `Cant_kick_self
    | _, Some "cant_leave_general" -> `Cant_leave_general
    | _, Some "cant_leave_last_channel" -> `Cant_leave_last_channel
    | _, Some "cant_update_message" -> `Cant_update_message
    | _, Some "channel_not_found" -> `Channel_not_found
    | _, Some "edit_window_closed" -> `Edit_window_closed
    | _, Some "file_deleted" -> `File_deleted
    | _, Some "file_not_found" -> `File_not_found
    | _, Some "invalid_auth" -> `Invalid_auth
    | _, Some "invalid_client_id" -> `Invalid_client_id
    | _, Some "invalid_code" -> `Invalid_code
    | _, Some "invalid_name" -> `Invalid_name
    | _, Some "invalid_presence" -> `Invalid_presence
    | _, Some "invalid_ts_latest" -> `Invalid_ts_latest
    | _, Some "invalid_ts_oldest" -> `Invalid_ts_oldest
    | _, Some "is_archived" -> `Is_archived
    | _, Some "last_member" -> `Last_member
    | _, Some "last_ra_channel" -> `Last_restricted_channel
    | _, Some "message_not_found" -> `Message_not_found
    (* not supposed to happen *)
    | _, Some "msg_too_long" -> `Msg_too_long
    | _, Some "too_many_attachments" -> `Too_many_attachments
    | _, Some "name_taken" -> `Name_taken
    (* can't really happen *)
    | _, Some "no_channel" -> `No_channel
    (* can't really happen either *)
    | _, Some "no_text" -> `No_text
    | _, Some "not_archived" -> `Not_archived
    | _, Some "not_authed" -> `Not_authed
    | _, Some "not_authorized" -> `Not_authorized
    | _, Some "not_in_channel" -> `Not_in_channel
    | _, Some "paid_only" -> `Paid_only
    | _, Some "rate_limited" -> `Rate_limited
    | _, Some "restricted_action" -> `Restricted_action
    | _, Some "too_long" -> `Too_long
    | _, Some "unknown_type" -> `Unknown_type
    | _, Some "user_does_not_own_channel" -> `User_does_not_own_channel
    | _, Some "user_is_bot" -> `User_is_bot
    | _, Some "user_is_restricted" -> `User_is_restricted
    (* lolwat, I'm not making this up *)
    | _, Some "user_is_ultra_restricted" -> `User_is_ultra_restricted
    | _, Some "user_not_found" -> `User_not_found
    | _, Some "user_not_visible" -> `User_not_visible
    (* when the API changes and introduces new, yet unhandled error types *)
    | _, Some err -> `Unhandled_error err
    | _ -> `Unknown_error

(* filter out "ok" and "error" keys *)
let filter_useless = function
  | `Json_response `Assoc items -> `Json_response (
      `Assoc (List.filter (fun (k, _) -> k <> "ok" && k <> "error") items))
  | otherwise -> otherwise

let process request =
  request
  >|= snd
  >>= Cohttp_body.to_string
  >|= Yojson.Safe.from_string
  >|= validate
  >|= filter_useless

let auth_header token =
  Cohttp.Header.init_with "Authorization" ("Bearer " ^ token)

let endpoint base_url request =
  let url = Uri.of_string (base_url ^ request.method') in
  List.fold_left (Uri.add_query_param') url request.arguments

let unauthed_query ?(base_url=default_base_url) request =
  endpoint base_url request
  |> Cohttp_unix.Client.get
  |> process

let query session request =
  endpoint session.base_url request
  |> Cohttp_unix.Client.get ~headers:(auth_header session.token)
  |> process

(* do a POST request *)
let query_post session body request =
  endpoint session.base_url request
  |> Cohttp_unix.Client.post ~headers:(auth_header session.token) ~body
  |> process

let identity x = x

let maybe fn = function
  | Some v -> Some (fn v)
  | None -> None

(* nonpublic types for conversion in list types *)
type channels_list_obj = {
  channels: channel_obj list
} [@@deriving of_yojson]

type users_list_obj = {
  members: user_obj list
} [@@deriving of_yojson { strict = false }]

type groups_list_obj = {
  groups: group_obj list;
} [@@deriving of_yojson]

type im_list_obj = {
  ims: im_obj list;
} [@@deriving of_yojson]

let channels_list ?exclude_archived session =
  api_request "channels.list"
    |> optionally_add "exclude_archived" @@ maybe string_of_bool @@ exclude_archived
    |> query session
    >|= function
    | `Json_response d ->
      (match d |> channels_list_obj_of_yojson with
        | Ok x -> `Success x.channels
        | Error x -> `ParseFailure x)
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

let users_list session =
  api_request "users.list"
    |> query session
    >|= function
    | `Json_response d ->
      (match d |> users_list_obj_of_yojson with
        | Ok x -> `Success x.members
        | Error x -> `ParseFailure x)
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

let groups_list ?exclude_archived session =
  api_request "groups.list"
    |> optionally_add "exclude_archived" @@ maybe string_of_bool exclude_archived
    |> query session
    >|= function
    | `Json_response d ->
      (match d |> groups_list_obj_of_yojson with
        | Ok x -> `Success x.groups
        | Error x -> `ParseFailure x)
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

type 'a listfn = session -> [`Success of 'a list | parsed_auth_error] Lwt.t

(* look up the id of query from results provided by the listfn *)
let lookupk session (listfn : 'a listfn) filterfn k =
  let* v = listfn session in
  match v with
  | #parsed_auth_error as e -> Lwt.return e
  | `Success items -> Lwt.return @@ k @@ List.filter filterfn items

let id_of_channel session = function
  | ChannelId id -> Lwt.return @@ `Found id
  | ChannelName name ->
    let base = String.sub name 1 @@ String.length name - 1 in
    lookupk session channels_list (fun (x:channel_obj) -> x.name = base) @@ function
    | [] -> `Channel_not_found
    | [{id = ChannelId s; _}] -> `Found s
    | [_] -> failwith "Bad result from channel id lookup."
    | _::_::_ -> failwith "Too many results from channel id lookup."

(* like id_of_channel but does not resolve names to ids *)
let string_of_channel = function
  | ChannelId id -> id
  | ChannelName name -> name

let id_of_user session = function
  | UserId id -> Lwt.return @@ `Found id
  | UserName name ->
    lookupk session users_list (fun (x:user_obj) -> x.name = name) @@ function
    | [] -> `User_not_found
    | [{id = UserId s; _}] -> `Found s
    | [_] -> failwith "Bad result from user id lookup."
    | _::_::_ -> failwith "Too many results from user id lookup."

let id_of_group session = function
  | GroupId id -> Lwt.return @@ `Found id
  | GroupName name ->
    lookupk session groups_list (fun (x:group_obj) -> x.name = name) @@ function
    | [] -> `Channel_not_found
    | [{id = GroupId s; _}] -> `Found s
    | [_] -> failwith "Bad result from group id lookup."
    | _::_::_ -> failwith "Too many results from group id lookup."

let id_of_chat session = function
  | Channel c -> id_of_channel session c
  | Im i -> Lwt.return @@ `Found i
  | User u -> id_of_user session u
  | Group g -> id_of_group session g

let name_of_group = function
  | GroupId _ -> failwith "Need to specify a name"
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
  | Auto -> "auto"
  | Away -> "away"

(* Slacko API helper methods *)
let start_session ?(base_url=default_base_url) token = {
  base_url;
  token;
}
let token_of_string token = start_session token
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

(* TODO Create a im if im does not exist? *)
let im_of_string s =
  if s.[0] = 'D' then s else failwith "Not an IM channel"

let translate_parsing_error = function
  | Error a -> `ParseFailure a
  | Ok a -> `Success a

(* Slack API begins here *)

let api_test ?(base_url=default_base_url) ?foo ?error () =
  api_request "api.test"
    |> optionally_add "foo" foo
    |> optionally_add "error" error
    |> unauthed_query ~base_url
    >|= function
    | `Json_response x -> `Success x
    | #api_error as res -> res
    | _ -> `Unknown_error

let auth_test session =
  api_request "auth.test"
    |> query session
    >|= function
    | `Json_response d -> d |> authed_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

(* Operator for unwrapping channel_ids *)
let (|->) m f =
  let* m = m in
  match m with
  | `Channel_not_found
  | #parsed_auth_error as e -> Lwt.return e
  | `User_not_found -> Lwt.return `Unknown_error
  | `Found v -> f v

(* Operator for unwrapping user_ids *)
let (|+>) m f =
  let* m = m in
  match m with
  | `Channel_not_found -> Lwt.return `Unknown_error
  | `User_not_found
  | #parsed_auth_error as e -> Lwt.return e
  | `Found v -> f v

let channels_archive session channel =
  id_of_channel session channel |-> fun channel_id ->
    api_request "channels.archive"
    |> definitely_add "channel" channel_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #already_archived_error
    | `Cant_archive_general
    | `Last_restricted_channel
    | #restriction_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let channels_create session name =
  api_request "channels.create"
    |> definitely_add "name" name
    |> query session
    >|= function
    | `Json_response (`Assoc [("channel", d)]) ->
        d |> channel_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error
    | #name_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let channels_history session
  ?latest ?oldest ?count ?inclusive channel =
  id_of_channel session channel |-> fun channel_id ->
    api_request "channels.history"
    |> definitely_add "channel" channel_id
    |> optionally_add "latest" @@ maybe Timestamp.to_string latest
    |> optionally_add "oldest" @@ maybe Timestamp.to_string oldest
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "inclusive" @@ maybe string_of_bool inclusive
    |> query session
    >|= function
    | `Json_response d -> d |> history_obj_of_yojson |> translate_parsing_error
    | #history_result as res -> res
    | _ -> `Unknown_error

let channels_info session channel =
  id_of_channel session channel |-> fun channel_id ->
    api_request "channels.info"
    |> definitely_add "channel" channel_id
    |> query session
    >|= function
    | `Json_response (`Assoc [("channel", d)]) ->
        d |> channel_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error as res -> res
    | _ -> `Unknown_error

let channels_invite session channel user =
  id_of_channel session channel |-> fun channel_id ->
  id_of_user session user |+> fun user_id ->
    api_request "channels.invite"
    |> definitely_add "channel" channel_id
    |> definitely_add "user" user_id
    |> query session
    >|= function
    | `Json_response (`Assoc [("channel", d)]) ->
        d |> channel_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #user_error
    | #bot_error
    | #invite_error
    | #not_in_channel_error
    | #already_in_channel_error
    | #archive_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error

let channels_join session name =
  api_request "channels.join"
    |> definitely_add "name" @@ string_of_channel name
    |> query session
    >|= function
    | `Json_response (`Assoc [("channel", d)]) ->
        d |> channel_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #name_error
    | #archive_error
    | #bot_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let channels_kick session channel user =
  id_of_channel session channel |-> fun channel_id ->
  id_of_user session user |+> fun user_id ->
    api_request "channels.kick"
    |> definitely_add "channel" channel_id
    |> definitely_add "user" user_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #user_error
    | #bot_error
    | #channel_kick_error
    | #not_in_channel_error
    | #restriction_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let channels_leave session channel =
  id_of_channel session channel |-> fun channel_id ->
  api_request "channels.leave"
    |> definitely_add "channel" channel_id
    |> query session
    >|= function
    | `Json_response d ->
      d |> channel_leave_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #archive_error
    | #leave_general_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let channels_mark session channel ts =
  id_of_channel session channel |-> fun channel_id ->
  api_request "channels.mark"
    |> definitely_add "channel" channel_id
    |> definitely_add "ts" @@ Timestamp.to_string ts
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #archive_error
    | #not_in_channel_error as res -> res
    | _ -> `Unknown_error

let channels_rename session channel name =
  id_of_channel session channel |-> fun channel_id ->
  api_request "channels.rename"
    |> definitely_add "channel" channel_id
    |> definitely_add "name" name
    |> query session
    >|= function
    | `Json_response (`Assoc [("channel", d)]) ->
        d |> channel_rename_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #not_in_channel_error
    | #name_error
    | #invalid_name_error
    | `Not_authorized
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let channels_set_purpose session channel purpose =
  id_of_channel session channel |-> fun channel_id ->
  api_request "channels.setPurpose"
    |> definitely_add "channel" channel_id
    |> definitely_add "purpose" purpose
    |> query session
    >|= function
    | `Json_response (`Assoc [("purpose", `String d)]) ->
      `Success d
    | #topic_result as res -> res
    | _ -> `Unknown_error

let channels_set_topic session channel topic =
  id_of_channel session channel |-> fun channel_id ->
  api_request "channels.setTopic"
    |> definitely_add "channel" channel_id
    |> definitely_add "topic" topic
    |> query session
    >|= function
    | `Json_response (`Assoc [("topic", `String d)]) ->
      `Success d
    | #topic_result as res -> res
    | _ -> `Unknown_error

let channels_unarchive session channel =
  id_of_channel session channel |-> fun channel_id ->
  api_request "channels.unarchive"
    |> definitely_add "channel" channel_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | `Not_archived
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let chat_delete session ts chat =
  id_of_chat session chat |-> fun chat_id ->
  api_request "chat.delete"
    |> definitely_add "channel" chat_id
    |> definitely_add "ts" @@ Timestamp.to_string ts
    |> query session
    >|= function
    | `Json_response d -> d |> chat_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #message_error as res -> res
    | _ -> `Unknown_error

let jsonify_attachments attachments =
  `List (List.map (fun a -> attachment_obj_to_yojson a) attachments)
  |> Yojson.Safe.to_string

let chat_post_message session chat
  ?as_user ?link_names ?mrkdwn
  ?reply_broadcast ?thread_ts ?unfurl_links ?unfurl_media
  ?username ?parse ?icon_url ?icon_emoji ?(attachments=[]) text =
  id_of_chat session chat |-> fun chat_id ->
  api_request "chat.postMessage"
    |> definitely_add "channel" chat_id
    |> definitely_add "text" text
    |> optionally_add "username" username
    |> optionally_add "parse" parse
    |> optionally_add "icon_url" icon_url
    |> optionally_add "icon_emoji" icon_emoji
    |> definitely_add "attachments" @@ jsonify_attachments attachments
    |> optionally_add "as_user" @@ maybe string_of_bool as_user
    |> optionally_add "link_names" @@ maybe string_of_bool link_names
    |> optionally_add "mrkdwn" @@ maybe string_of_bool mrkdwn
    |> optionally_add "reply_broadcast" @@ maybe string_of_bool reply_broadcast
    |> optionally_add "thread_ts" @@ maybe Timestamp.to_string thread_ts
    |> optionally_add "unfurl_links" @@ maybe string_of_bool unfurl_links
    |> optionally_add "unfurl_media" @@ maybe string_of_bool unfurl_media
    |> query session
    >|= function
    | `Json_response d ->
      d |> chat_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #archive_error
    | #message_length_error
    | #attachments_error
    | #rate_error as res -> res
    | _ -> `Unknown_error

let chat_update session ts chat ?as_user ?attachments ?link_names ?parse text =
  id_of_chat session chat |-> fun chat_id ->
  api_request "chat.update"
    |> definitely_add "channel" chat_id
    |> definitely_add "ts" @@ Timestamp.to_string ts
    |> definitely_add "text" text
    |> optionally_add "as_user" @@ maybe string_of_bool as_user
    |> optionally_add "attachments" @@ maybe jsonify_attachments attachments
    |> optionally_add "link_names" @@ maybe string_of_bool link_names
    |> optionally_add "parse" parse
    |> query session
    >|= function
    | `Json_response d ->
      d |> chat_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #message_update_error
    | #message_length_error
    | #attachments_error as res -> res
    | _ -> `Unknown_error

let emoji_list session =
  api_request "emoji.list"
    |> query session
    >|= function
    | `Json_response d ->
      (match d |> emoji_list_obj_of_yojson with
      | Ok x -> `Success x.emoji
      | Error x -> `ParseFailure x)
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

let files_delete session file =
  api_request "files.delete"
    |> definitely_add "file" file
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #bot_error
    | `Cant_delete_file
    | #file_error as res -> res
    | _ -> `Unknown_error

let files_info session ?count ?page file =
  api_request "files.info"
    |> definitely_add "file" file
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query session
    >|= function
    | `Json_response d ->
      d |> files_info_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error
    | #file_error as res -> res
    | _ -> `Unknown_error

let maybe_with_user session user f =
  match user with
    | Some u -> id_of_user session u >>= (function
        | `Found id -> f @@ Some id
        | _ -> Lwt.return `User_not_found)
    | None -> f None

let files_list ?user ?ts_from ?ts_to ?types ?count ?page session =
  maybe_with_user session user @@ fun user_id ->
  api_request "files.list"
    |> optionally_add "user" user_id
    |> optionally_add "ts_from" @@ maybe Timestamp.to_string ts_from
    |> optionally_add "ts_to" @@ maybe Timestamp.to_string ts_to
    |> optionally_add "types" types
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query session
    >|= function
    | `Json_response d ->
      d |> files_list_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #user_error
    | #bot_error
    | #unknown_type_error as res -> res
    | _ -> `Unknown_error

let files_upload session
  ?filetype ?filename ?title ?initial_comment ?channels content =
  api_request "files.upload"
    |> optionally_add "filetype" filetype
    |> optionally_add "filename" filename
    |> optionally_add "title" title
    |> optionally_add "initial_comment" initial_comment
    |> optionally_add "channels" channels
    |> query_post session content
    >|= function
    | `Json_response `Assoc [("file", d)] ->
        d |> file_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error as res -> res
    | _ -> `Unknown_error

let groups_archive session group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.archive"
    |> definitely_add "channel" group_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #already_archived_error
    | `Group_contains_others
    | `Last_restricted_channel
    | #restriction_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error

let groups_close session group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.close"
    |> definitely_add "channel" group_id
    |> query session
    >|= function
    | `Json_response d ->
      d |> chat_close_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error as res -> res
    | _ -> `Unknown_error

let groups_create session name =
  api_request "groups.create"
    |> definitely_add "name" @@ name_of_group name
    |> query session
    >|= function
    | `Json_response (`Assoc [("group", d)]) ->
      d |> group_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error
    | #name_error
    | #restriction_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error

let groups_create_child session group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.createChild"
    |> definitely_add "channel" group_id
    |> query session
    >|= function
    | `Json_response (`Assoc [("group", d)]) ->
      d |> group_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #already_archived_error
    | #restriction_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error

let groups_history session ?latest ?oldest ?count ?inclusive group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.history"
    |> definitely_add "channel" group_id
    |> optionally_add "latest" @@ maybe Timestamp.to_string latest
    |> optionally_add "oldest" @@ maybe Timestamp.to_string oldest
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "inclusive" @@ maybe string_of_bool inclusive
    |> query session
    >|= function
    | `Json_response d -> d |> history_obj_of_yojson |> translate_parsing_error
    | #history_result as res -> res
    | _ -> `Unknown_error

let groups_invite session group user =
  id_of_group session group |-> fun group_id ->
  id_of_user session user |+> fun user_id ->
  api_request "groups.invite"
    |> definitely_add "channel" group_id
    |> definitely_add "user" user_id
    |> query session
    >|= function
    | `Json_response d ->
      d |> groups_invite_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #user_error
    | #bot_error
    | #invite_error
    | #archive_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error

let groups_kick session group user =
  id_of_group session group |-> fun group_id ->
  id_of_user session user |+> fun user_id ->
  api_request "groups.kick"
    |> definitely_add "channel" group_id
    |> definitely_add "user" user_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #user_error
    | #bot_error
    | #kick_error
    | #not_in_group_error
    | #restriction_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let groups_leave session group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.leave"
    |> definitely_add "channel" group_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #archive_error
    | #leave_last_channel_error
    | #last_member_error
    | `User_is_ultra_restricted as res -> res
    | _ -> `Unknown_error

let groups_mark session group ts =
  id_of_group session group |-> fun group_id ->
  api_request "groups.mark"
    |> definitely_add "channel" group_id
    |> definitely_add "ts" @@ Timestamp.to_string ts
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #archive_error
    | #not_in_channel_error as res -> res
    | _ -> `Unknown_error

let groups_open session group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.open"
    |> definitely_add "channel" group_id
    |> query session
    >|= function
    | `Json_response d ->
      d |> groups_open_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error as res -> res
    | _ -> `Unknown_error

let groups_rename session group name =
  id_of_group session group |-> fun group_id ->
  api_request "groups.rename"
    |> definitely_add "channel" group_id
    |> definitely_add "name" name
    |> query session
    >|= function
    | `Json_response (`Assoc [("channel", d)]) ->
      d |> groups_rename_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | #name_error
    | #invalid_name_error
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let groups_set_purpose session group purpose =
  id_of_group session group |-> fun group_id ->
  api_request "groups.setPurpose"
    |> definitely_add "channel" group_id
    |> definitely_add "purpose" purpose
    |> query session
    >|= function
    | `Json_response (`Assoc [("purpose", `String d)]) ->
      `Success d
    | #topic_result as res -> res
    | _ -> `Unknown_error

let groups_set_topic session group topic =
  id_of_group session group |-> fun group_id ->
  api_request "groups.setTopic"
    |> definitely_add "channel" group_id
    |> definitely_add "topic" topic
    |> query session
    >|= function
    | `Json_response (`Assoc [("topic", `String d)]) ->
      `Success d
    | #topic_result as res -> res
    | _ -> `Unknown_error

let groups_unarchive session group =
  id_of_group session group |-> fun group_id ->
  api_request "groups.unarchive"
    |> definitely_add "channel" group_id
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #bot_error
    | `Not_archived
    | `User_is_restricted as res -> res
    | _ -> `Unknown_error

let im_close session channel =
  api_request "im.close"
    |> definitely_add "channel" channel
    |> query session
    >|= function
    | `Json_response d ->
      d |> chat_close_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #channel_error
    | `User_does_not_own_channel as res -> res
    | _ -> `Unknown_error

let im_history session ?latest ?oldest ?count ?inclusive channel =
  api_request "im.history"
    |> definitely_add "channel" channel
    |> optionally_add "latest" @@ maybe Timestamp.to_string latest
    |> optionally_add "oldest" @@ maybe Timestamp.to_string oldest
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "inclusive" @@ maybe string_of_bool inclusive
    |> query session
    >|= function
    | `Json_response d -> d |> history_obj_of_yojson |> translate_parsing_error
    | #history_result as res -> res
    | _ -> `Unknown_error

let im_list session =
  api_request "im.list"
    |> query session
    >|= function
    | `Json_response d ->
      (match d |> im_list_obj_of_yojson with
        | Ok x -> `Success x.ims
        | Error x -> `ParseFailure x)
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

let im_mark session channel ts =
  api_request "im.mark"
    |> definitely_add "channel" channel
    |> definitely_add "ts" @@ Timestamp.to_string ts
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #channel_error
    | #not_in_channel_error as res -> res
    | _ -> `Unknown_error

let im_open session user =
  id_of_user session user |+> fun user_id ->
  api_request "im.open"
    |> definitely_add "user" user_id
    |> query session
    >|= function
    | `Json_response d ->
      d |> im_open_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #user_error
    | #user_visibility_error as res -> res
    | _ -> `Unknown_error

let oauth_access ?(base_url=default_base_url) client_id client_secret ?redirect_url code =
  api_request "oauth.access"
    |> definitely_add "client_id" client_id
    |> definitely_add "client_secret" client_secret
    |> definitely_add "code" code
    |> optionally_add "redirect_url" redirect_url
    |> unauthed_query ~base_url
    >|= function
    | `Json_response d ->
      d |> oauth_obj_of_yojson |> translate_parsing_error
    | #oauth_error as res -> res
    | _ -> `Unknown_error

let search method' session ?sort ?sort_dir ?highlight ?count ?page query_ =
  api_request method'
    |> definitely_add "query" @@ query_
    |> optionally_add "sort" @@ maybe string_of_criterion sort
    |> optionally_add "sort_dir" @@ maybe string_of_direction sort_dir
    |> optionally_add "highlight" @@ maybe string_of_bool highlight
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query session
    >|= function
    | `Json_response d ->
      d |> search_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error as res -> res
    | _ -> `Unknown_error

let search_all = search "search.all"
let search_files = search "search.files"
let search_messages = search "search.messages"

let stars_list ?user ?count ?page session =
  maybe_with_user session user @@ fun user_id ->
  api_request "stars.list"
    |> optionally_add "user" user_id
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query session
    >|= function
    | `Json_response d ->
      d |> stars_list_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error
    | #user_error as res -> res
    | _ -> `Unknown_error

let team_access_logs ?count ?page session =
  api_request "team.accessLogs"
    |> optionally_add "count" @@ maybe string_of_int count
    |> optionally_add "page" @@ maybe string_of_int page
    |> query session
    >|= function
    | `Json_response d ->
      d |> team_access_log_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | `Paid_only
    | #bot_error as res -> res
    | _ -> `Unknown_error

let team_info session =
  api_request "team.info"
    |> query session
    >|= function
    | `Json_response d ->
      d |> team_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #bot_error as res -> res
    | _ -> `Unknown_error

let users_get_presence session user =
  id_of_user session user |+> fun user_id ->
  api_request "users.getPresence"
    |> definitely_add "user" user_id
    |> query session
    >|= function
    (* TODO parse more out of this *)
    | `Json_response (`Assoc [("presence", `String d)]) -> (match d with
      | "active" -> `Success Auto
      | "away" -> `Success Away
      | _ -> `ParseFailure "Invalid presence")
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

let users_info session user =
  id_of_user session user
  |+> fun user_id ->
  api_request "users.info"
    |> definitely_add "user" user_id
    |> query session
    >|= function
    | `Json_response (`Assoc [("user", d)]) ->
        d |> user_obj_of_yojson |> translate_parsing_error
    | #parsed_auth_error
    | #user_error
    | #user_visibility_error as res -> res
    | _ -> `Unknown_error

let users_set_active session =
  api_request "users.setActive"
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #bot_error
    | #parsed_auth_error as res -> res
    | _ -> `Unknown_error

let users_set_presence session presence =
  api_request "users.setPresence"
    |> definitely_add "presence" @@ string_of_presence presence
    |> query session
    >|= function
    | `Json_response (`Assoc []) -> `Success
    | #parsed_auth_error
    | #presence_error as res -> res
    | _ -> `Unknown_error
