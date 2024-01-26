open Lwt
open Abbrtypes

let token =
  try Sys.getenv "SLACKO_TEST_TOKEN" with Not_found -> Fake_slack.valid_token

let badtoken = "badtoken"
let yojson = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

(* If we have a non-default token, assume we want to talk to real slack. If
   not, use our local fake instead. *)
let base_url =
  match token with
  | t when t = Fake_slack.valid_token -> Some "http://127.0.0.1:7357/api/"
  | _ -> (
      print_endline
        ("NOTE: Because an API token has been provided, "
       ^ "tests will run against the real slack API.");
      try
        (* We may want to talk to a proxy or a different fake slack. *)
        let base_url = Sys.getenv "SLACKO_TEST_BASE_URL" in
        print_endline @@ "NOTE: Overriding slack base URL to " ^ base_url;
        Some base_url
      with Not_found -> None)

let abbr_json abbr_of_yojson json =
  match abbr_of_yojson json with
  | Ok abbr -> abbr
  | Error err -> failwith @@ "Error parsing JSON: " ^ err

let get_success = function
  | `Success obj -> obj
  | _ -> Alcotest.fail "Unexpected failure."

(* api_test *)

let test_api_test_nodata () =
  Slacko.api_test ?base_url () >|= get_success >|= fun json ->
  Alcotest.(check yojson) "api.test empty" (`Assoc []) json

let test_api_test_foo () =
  Slacko.api_test ?base_url ~foo:"hello" () >|= get_success >|= fun json ->
  Alcotest.(check yojson)
    "api.test foo"
    (`Assoc [ ("args", `Assoc [ ("foo", `String "hello") ]) ])
    json

type api_error = [ `Unhandled_error of string | `Unknown_error ]
[@@deriving show, eq]

type api_test = [ `Success of Yojson.Safe.t | api_error ] [@@deriving show, eq]

let api_test = Alcotest.testable pp_api_test equal_api_test

let test_api_test_err () =
  Slacko.api_test ?base_url ~error:"badthing" () >|= fun resp ->
  Alcotest.(check api_test) "error" (`Unhandled_error "badthing") resp

let test_api_test_err_foo () =
  Slacko.api_test ?base_url ~foo:"goodbye" ~error:"badthing" () >|= fun resp ->
  Alcotest.(check api_test) "error foo" (`Unhandled_error "badthing") resp

let test_set ~label cases =
  let open Alcotest_lwt in
  let tests =
    List.map
      (fun (name, f) ->
        test_case name `Quick (fun _sw () -> Fake_slack.with_fake_slack f))
      cases
  in
  (label, tests)

let api_test_tests =
  test_set ~label:"api_test"
    [
      ("nodata", test_api_test_nodata);
      ("foo", test_api_test_foo);
      ("err", test_api_test_err);
      ("err_fooo", test_api_test_err_foo);
    ]

(* auth_test *)

let abbr_authed_obj' =
  Alcotest.testable pp_abbr_authed_obj equal_abbr_authed_obj

let test_auth_test_valid () =
  let session = Slacko.start_session ?base_url token in
  Slacko.auth_test session >|= get_success >|= abbr_authed_obj >|= fun authed ->
  Alcotest.(check abbr_authed_obj')
    "valid"
    (abbr_json abbr_authed_obj_of_yojson Fake_slack.authed_json)
    authed

type parsed_api_error = [ `ParseFailure of string | api_error ]
[@@deriving show, eq]

type auth_error = [ `Not_authed | `Invalid_auth | `Account_inactive ]
[@@deriving show, eq]

type parsed_auth_error = [ parsed_api_error | auth_error ] [@@deriving show, eq]
type user = Slacko.user

let pp_user ppf _v : unit = Fmt.pf ppf "TODO"
let equal_user _ _ = true

type authed_obj = Slacko.authed_obj = {
  url : string;
  team : string;
  user : string;
  team_id : string;
  user_id : user;
}
[@@deriving show, eq]

type auth_test = [ `Success of authed_obj | parsed_auth_error ]
[@@deriving show, eq]

let auth_test = Alcotest.testable pp_auth_test equal_auth_test

let test_auth_test_invalid () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.auth_test session >|= fun resp ->
  Alcotest.(check auth_test) "invalid auth" `Invalid_auth resp

let auth_test_tests =
  test_set ~label:"test_auth"
    [
      ("test_valid", test_auth_test_valid);
      ("test_invalid", test_auth_test_invalid);
    ]

(* channels_archive  *)

type channel_error = [ `Channel_not_found ] [@@deriving show, eq]
type already_archived_error = [ `Already_archived ] [@@deriving show, eq]
type restriction_error = [ `Restricted_action ] [@@deriving show, eq]
type bot_error = [ `User_is_bot ] [@@deriving show, eq]

type channels_archive =
  [ `Success
  | parsed_auth_error
  | channel_error
  | already_archived_error
  | `Cant_archive_general
  | `Last_restricted_channel
  | restriction_error
  | `User_is_restricted
  | bot_error ]
[@@deriving show, eq]

let channels_archive =
  Alcotest.testable pp_channels_archive equal_channels_archive

let test_channels_archive_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  let new_channel = Slacko.channel_of_string "new_channel" in
  Slacko.channels_archive session new_channel >|= fun resp ->
  Alcotest.(check channels_archive) "bad auth" `Invalid_auth resp

let test_channels_archive_existing () =
  let session = Slacko.start_session ?base_url token in
  let new_channel = Slacko.channel_of_string "archivable_channel" in
  Slacko.channels_archive session new_channel >|= fun resp ->
  Alcotest.(check channels_archive) "existing" `Success resp

let test_channels_archive_missing () =
  let session = Slacko.start_session ?base_url token in
  let missing_channel = Slacko.channel_of_string "missing_channel" in
  Slacko.channels_archive session missing_channel >|= fun resp ->
  Alcotest.(check channels_archive) "not found" `Channel_not_found resp

let test_channels_archive_archived () =
  let session = Slacko.start_session ?base_url token in
  let archived_channel = Slacko.channel_of_string "archived_channel" in
  Slacko.channels_archive session archived_channel >|= fun resp ->
  Alcotest.(check channels_archive) "already archived" `Already_archived resp

let test_channels_archive_general () =
  let session = Slacko.start_session ?base_url token in
  let general = Slacko.channel_of_string "general" in
  Slacko.channels_archive session general >|= fun resp ->
  Alcotest.(check channels_archive)
    "can't archive general" `Cant_archive_general resp

let channels_archive_tests =
  test_set ~label:"channels_archive"
    [
      ("test_bad_auth", test_channels_archive_bad_auth);
      ("test_existing", test_channels_archive_existing);
      ("test_missing", test_channels_archive_missing);
      ("test_archived", test_channels_archive_archived);
      ("test_general", test_channels_archive_general);
    ]

(* channels_create *)

type channel = Slacko.channel

let pp_channel ppf _ = Fmt.pf ppf "TODO"
let equal_channel _ _ = true

type topic_obj = Slacko.topic_obj = {
  value : string;
  creator : user;
  last_set : Timestamp.t;
}
[@@deriving show, eq]

type channel_obj = Slacko.channel_obj = {
  id : channel;
  name : string;
  is_channel : bool;
  created : Timestamp.t;
  creator : user;
  is_archived : bool;
  is_general : bool;
  name_normalized : string;
  is_member : bool;
  members : user list;
  topic : topic_obj;
  purpose : topic_obj;
  last_read : Timestamp.t option;
  latest : Yojson.Safe.t option;
  unread_count : int option;
  unread_count_display : int option;
  num_members : int option;
}
[@@deriving show, eq]

type name_error = [ `Name_taken ] [@@deriving show, eq]

type channels_create =
  [ `Success of channel_obj
  | parsed_auth_error
  | name_error
  | `User_is_restricted
  | bot_error ]
[@@deriving show, eq]

let channels_create = Alcotest.testable pp_channels_create equal_channels_create

let abbr_channel_obj' =
  Alcotest.testable pp_abbr_channel_obj equal_abbr_channel_obj

let test_channels_create_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.channels_create session "new_channel" >|= fun resp ->
  Alcotest.(check channels_create) "new channel bad auth" `Invalid_auth resp

let test_channels_create_new () =
  let session = Slacko.start_session ?base_url token in
  Slacko.channels_create session "new_channel"
  >|= get_success >|= abbr_channel_obj
  >|= fun channel ->
  Alcotest.(check abbr_channel_obj')
    "new channel"
    (abbr_json abbr_channel_obj_of_yojson Fake_slack.new_channel_json)
    channel

let test_channels_create_existing () =
  let session = Slacko.start_session ?base_url token in
  Slacko.channels_create session "general" >|= fun resp ->
  Alcotest.(check channels_create) "existing" `Name_taken resp

let channels_create_tests =
  test_set ~label:"channels_create"
    [
      ("test_bad_auth", test_channels_create_bad_auth);
      ("test_new", test_channels_create_new);
      ("test_existing", test_channels_create_existing);
    ]

(* channels_history *)

type bot = Slacko.bot

let pp_bot ppf _ = Fmt.pf ppf "TODO"
let equal_bot _ _ = true

type message_obj = Slacko.message_obj = {
  type' : string;
  ts : Timestamp.t;
  user : user option;
  bot_id : bot option;
  text : string option;
  is_starred : bool option;
}
[@@deriving show, eq]

type history_obj = Slacko.history_obj = {
  latest : Timestamp.t option;
  messages : message_obj list;
  has_more : bool;
}
[@@deriving show, eq]

type timestamp_error = [ `Invalid_ts_latest | `Invalid_ts_oldest ]
[@@deriving show, eq]

type history_result =
  [ `Success of history_obj
  | parsed_auth_error
  | channel_error
  | timestamp_error ]
[@@deriving show, eq]

let history_result = Alcotest.testable pp_history_result equal_history_result

let abbr_history_obj' =
  Alcotest.testable pp_abbr_history_obj equal_abbr_history_obj

let test_channels_history_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  let new_channel = Slacko.channel_of_string "new_channel" in
  Slacko.channels_history session new_channel >|= fun resp ->
  Alcotest.(check history_result) "bad auth" `Invalid_auth resp

let test_channels_history_no_params () =
  let session = Slacko.start_session ?base_url token in
  let random = Slacko.channel_of_string "random" in
  Slacko.channels_history session random >|= get_success >|= fun history ->
  Alcotest.(check abbr_history_obj')
    "no params"
    (abbr_json abbr_history_obj_of_yojson Fake_slack.random_history_json)
    (abbr_history_obj history)

let channels_history_tests =
  test_set ~label:"channels_history"
    [
      ("test_bad_auth", test_channels_history_bad_auth);
      ("test_no_params", test_channels_history_no_params);
    ]

(* channels_info *)
(* channels_invite *)
(* channels_join *)
(* channels_kick *)
(* channels_leave *)
type conversation = Slacko.conversation

let pp_conversation ppf _ = Fmt.pf ppf "TODO"
let equal_conversation _ _ = true

(* conversations_list *)
type conversation_obj = Slacko.conversation_obj = {
  id : conversation;
  name : string;
  is_channel : bool;
  created : Timestamp.t;
  creator : user;
  is_archived : bool;
  is_general : bool;
  name_normalized : string;
  is_member : bool;
  topic : topic_obj;
  purpose : topic_obj;
  last_read : Timestamp.t option;
  latest : string option;
  unread_count : int option;
  unread_count_display : int option;
  num_members : int option;
}
[@@deriving show, eq]

type conversations_list =
  [ `Success of conversation_obj list | parsed_auth_error ]
[@@deriving show, eq]

let conversations_list =
  Alcotest.testable pp_conversations_list equal_conversations_list

let test_conversations_list_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.conversations_list session >|= fun resp ->
  Alcotest.(check conversations_list) "bad auth" `Invalid_auth resp

let abbr_conversation_obj_list' =
  Alcotest.testable pp_abbr_conversation_obj_list
    equal_abbr_conversation_obj_list

let test_conversations_list () =
  let session = Slacko.start_session ?base_url token in
  Slacko.conversations_list session
  >|= get_success
  >|= List.map abbr_conversation_obj
  >|= fun conversations ->
  Alcotest.(check abbr_conversation_obj_list')
    "test"
    (abbr_json abbr_conversation_obj_list_of_yojson
       Fake_slack.conversations_json)
    conversations

let conversations_list_tests =
  test_set ~label:"conversations_list"
    [
      ("test_bad_auth", test_conversations_list_bad_auth);
      ("test", test_conversations_list);
    ]

(* channels_mark *)
(* channels_rename *)
(* channels_set_purpose *)
(* channels_set_topic *)
(* channels_unarchive *)
(* chat_delete *)
(* chat_post_message *)
(* chat_update *)
(* emoji_list *)
(* files_delete *)
(* files_info *)

(* files_list *)
type group = Slacko.group

let pp_group ppf _ = Fmt.pf ppf "TODO"
let equal_group _ _ = true

type im = Slacko.im

let pp_im ppf _ = Fmt.pf ppf "TODO"
let equal_im _ _ = true

type file_obj = Slacko.file_obj = {
  id : string;
  created : Timestamp.t;
  timestamp : Timestamp.t;
  name : string option;
  title : string;
  mimetype : string;
  pretty_type : string;
  user : user;
  mode : string;
  editable : bool;
  is_external : bool;
  external_type : string;
  size : int;
  url_private : string;
  url_private_download : string;
  thumb_64 : string option;
  thunb_80 : string option;
  thumb_360 : string option;
  thumb_360_gif : string option;
  thumb_360_w : int option;
  thumb_360_h : int option;
  permalink : string;
  edit_link : string option;
  preview : string option;
  preview_highlight : string option;
  lines : int option;
  lines_more : int option;
  is_public : bool;
  channels : channel list;
  groups : group list;
  ims : im list;
  initial_comment : Yojson.Safe.t option;
  num_stars : int option;
}
[@@deriving show, eq]

type paging_obj = Slacko.paging_obj = {
  count : int;
  total : int;
  page : int;
  pages : int;
}
[@@deriving show, eq]

type files_list_obj = Slacko.files_list_obj = {
  files : file_obj list;
  paging : paging_obj;
}
[@@deriving show, eq]

type user_error = [ `User_not_found ] [@@deriving show, eq]
type unknown_type_error = [ `Unknown_type ] [@@deriving show, eq]

type files_list =
  [ `Success of files_list_obj
  | parsed_auth_error
  | user_error
  | unknown_type_error
  | bot_error ]
[@@deriving show, eq]

let files_list = Alcotest.testable pp_files_list equal_files_list

let test_files_list_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.files_list session >|= fun resp ->
  Alcotest.(check files_list) "bad auth" `Invalid_auth resp

let abbr_files_list_obj' =
  Alcotest.testable pp_abbr_files_list_obj equal_abbr_files_list_obj

let test_files_list () =
  let session = Slacko.start_session ?base_url token in
  Slacko.files_list session >|= get_success >|= abbr_files_list_obj
  >|= fun files ->
  Alcotest.(check abbr_files_list_obj')
    "test"
    (abbr_json abbr_files_list_obj_of_yojson Fake_slack.files_json)
    files

let files_list_tests =
  test_set ~label:"files_list"
    [ ("test_bad_auth", test_files_list_bad_auth); ("test", test_files_list) ]

(* files_upload *)
(* groups_archive *)
(* groups_close *)
(* groups_create *)
(* groups_create_child *)

(* groups_history *)

let test_groups_history_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  let seekrit = Slacko.group_of_string "seekrit" in
  Slacko.groups_history session seekrit >|= fun resp ->
  Alcotest.(check history_result) "bad auth" `Invalid_auth resp

let test_groups_history_no_params () =
  let session = Slacko.start_session ?base_url token in
  let seekrit = Slacko.group_of_string "seekrit" in
  Slacko.groups_history session seekrit >|= get_success >|= fun history ->
  Alcotest.(check abbr_history_obj')
    "no params"
    (abbr_json abbr_history_obj_of_yojson Fake_slack.seekrit_history_json)
    (abbr_history_obj history)

let groups_history_tests =
  test_set ~label:"groups_history"
    [
      ("test_bad_auth", test_groups_history_bad_auth);
      ("test_no_params", test_groups_history_no_params);
    ]

(* groups_invite *)
(* groups_kick *)
(* groups_leave *)

(* groups_list *)
type group_obj = Slacko.group_obj = {
  id : group;
  name : string;
  is_group : bool;
  created : Timestamp.t;
  creator : user;
  is_archived : bool;
  members : user list;
  topic : topic_obj;
  purpose : topic_obj;
  is_open : bool option;
  last_read : Timestamp.t option;
  unread_count : int option;
  unread_count_display : int option;
  latest : Yojson.Safe.t option;
}
[@@deriving show, eq]

type groups_list = [ `Success of group_obj list | parsed_auth_error ]
[@@deriving show, eq]

let groups_list = Alcotest.testable pp_groups_list equal_groups_list

let test_groups_list_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.groups_list session >|= fun resp ->
  Alcotest.(check groups_list) "bad auth" `Invalid_auth resp

let abbr_group_obj_list =
  Alcotest.testable pp_abbr_group_obj_list equal_abbr_group_obj_list

let test_groups_list () =
  let session = Slacko.start_session ?base_url token in
  Slacko.groups_list session >|= get_success >|= List.map abbr_group_obj
  >|= fun groups ->
  Alcotest.(check abbr_group_obj_list)
    "test"
    (abbr_json abbr_group_obj_list_of_yojson Fake_slack.groups_json)
    groups

let groups_list_tests =
  test_set ~label:"groups_list"
    [ ("test_bad_auth", test_groups_list_bad_auth); ("test", test_groups_list) ]

(* groups_mark *)
(* groups_open *)
(* groups_rename *)
(* groups_set_purpose *)
(* groups_set_topic *)
(* groups_unarchive *)
(* im_close *)

(* im_history *)

let test_im_history_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  let slackbot = Slacko.im_of_string Fake_slack.im_slackbot in
  Slacko.im_history session slackbot >|= fun resp ->
  Alcotest.(check history_result) "bad auth" `Invalid_auth resp

let test_im_history_no_params () =
  let session = Slacko.start_session ?base_url token in
  let slackbot = Slacko.im_of_string Fake_slack.im_slackbot in
  Slacko.im_history session slackbot >|= get_success >|= fun history ->
  Alcotest.(check abbr_history_obj')
    "no params"
    (abbr_json abbr_history_obj_of_yojson Fake_slack.slackbot_history_json)
    (abbr_history_obj history)

let im_history_tests =
  test_set ~label:"im_history"
    [
      ("test_bad_auth", test_im_history_bad_auth);
      ("test_no_params", test_im_history_no_params);
    ]

(* im_list *)

type im_obj = Slacko.im_obj = {
  id : string;
  is_im : bool;
  user : user;
  created : Timestamp.t;
  is_user_deleted : bool;
  is_open : bool option;
  last_read : Timestamp.t option;
  unread_count : int option;
  unread_count_display : int option;
}
[@@deriving show, eq]

type im_list = [ `Success of im_obj list | parsed_auth_error ]
[@@deriving show, eq]

let im_list = Alcotest.testable pp_im_list equal_im_list

let test_im_list_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.im_list session >|= fun resp ->
  Alcotest.(check im_list) "bad auth" `Invalid_auth resp

let abbr_im_obj_list' =
  Alcotest.testable pp_abbr_im_obj_list equal_abbr_im_obj_list

let test_im_list () =
  let session = Slacko.start_session ?base_url token in
  Slacko.im_list session >|= get_success >|= List.map abbr_im_obj >|= fun ims ->
  Alcotest.check abbr_im_obj_list' "test"
    (abbr_json abbr_im_obj_list_of_yojson Fake_slack.ims_json)
    ims

let im_list_tests =
  test_set ~label:"im_list"
    [ ("test_bad_auth", test_im_list_bad_auth); ("test", test_im_list) ]

(* im_mark *)
(* im_open *)
(* oauth_access *)
(* search_all *)
(* search_files *)
(* search_messages *)
(* stars_list *)
(* team_access_logs *)
(* team_info *)
(* users_get_presence *)
(* users_info *)

(* users_list *)

type user_obj = Slacko.user_obj = {
  id : user;
  name : string;
  deleted : bool;
  color : string option;
  real_name : string option;
  tz : string option;
  tz_label : string option;
  tz_offset : int;
  profile : Yojson.Safe.t;
  is_admin : bool;
  is_owner : bool;
  is_primary_owner : bool;
  is_restricted : bool;
  is_ultra_restricted : bool;
  is_bot : bool;
  has_files : bool;
}
[@@deriving show, eq]

type users_list = [ `Success of user_obj list | parsed_auth_error ]
[@@deriving show, eq]

let users_list = Alcotest.testable pp_users_list equal_users_list

let test_users_list_bad_auth () =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.users_list session >|= fun resp ->
  Alcotest.check users_list "bad auth" `Invalid_auth resp

let abbr_user_obj_list =
  Alcotest.testable pp_abbr_user_obj_list equal_abbr_user_obj_list

let test_users_list () =
  let session = Slacko.start_session ?base_url token in
  Slacko.users_list session >|= get_success >|= List.map abbr_user_obj
  >|= fun users ->
  Alcotest.check abbr_user_obj_list "test"
    (abbr_json abbr_user_obj_list_of_yojson Fake_slack.users_json)
    users

let users_list_tests =
  test_set ~label:"users_list"
    [ ("test_bad_auth", test_users_list_bad_auth); ("test", test_users_list) ]

(* users_set_active *)
(* users_set_presence *)

(* Gotta run them all! *)

let suite =
  let open Alcotest_lwt in
  run "tests"
    [
      api_test_tests;
      auth_test_tests;
      channels_archive_tests;
      channels_create_tests;
      channels_history_tests;
      (* channels_info_tests; *)
      (* channels_invite_tests; *)
      (* channels_join_tests; *)
      (* channels_kick_tests; *)
      (* channels_leave_tests; *)
      conversations_list_tests;
      (* channels_mark_tests; *)
      (* channels_rename_tests; *)
      (* channels_set_purpose_tests; *)
      (* channels_set_topic_tests; *)
      (* channels_unarchive_tests; *)
      (* chat_delete_tests; *)
      (* chat_post_message_tests; *)
      (* chat_update_tests; *)
      (* emoji_list_tests; *)
      (* files_delete_tests; *)
      (* files_info_tests; *)
      files_list_tests;
      (* files_upload_tests; *)
      (* groups_archive_tests; *)
      (* groups_close_tests; *)
      (* groups_create_tests; *)
      (* groups_create_child_tests; *)
      groups_history_tests;
      (* groups_invite_tests; *)
      (* groups_kick_tests; *)
      (* groups_leave_tests; *)
      groups_list_tests;
      (* groups_mark_tests; *)
      (* groups_open_tests; *)
      (* groups_rename_tests; *)
      (* groups_set_purpose_tests; *)
      (* groups_set_topic_tests; *)
      (* groups_unarchive_tests; *)
      (* im_close_tests; *)
      im_history_tests;
      im_list_tests;
      (* im_mark_tests; *)
      (* im_open_tests; *)
      (* oauth_access_tests; *)
      (* search_all_tests; *)
      (* search_files_tests; *)
      (* search_messages_tests; *)
      (* stars_list_tests; *)
      (* team_access_logs_tests; *)
      (* team_info_tests; *)
      (* users_get_presence_tests; *)
      (* users_info_tests; *)
      users_list_tests;
      (* users_set_active_tests; *)
      (* users_set_presence_tests; *)
    ]

let () = Lwt_main.run suite
