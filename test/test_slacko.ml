open Lwt
open OUnit2

open Slounit
open Abbrtypes


let token =
  try Sys.getenv "SLACKO_TEST_TOKEN" with Not_found -> Fake_slack.valid_token

let badtoken = "badtoken"

(* If we have a non-default token, assume we want to talk to real slack. If
   not, use our local fake instead. *)
let base_url = match token with
  | t when t = Fake_slack.valid_token -> Some "http://127.0.0.1:7357/api/"
  | _ ->
    print_endline ("NOTE: Because an API token has been provided, " ^
                   "tests will run against the real slack API.");
    try
      (* We may want to talk to a proxy or a different fake slack. *)
      let base_url = Sys.getenv "SLACKO_TEST_BASE_URL" in
      print_endline @@ "NOTE: Overriding slack base URL to " ^ base_url;
      Some base_url;
    with Not_found -> None


let abbr_json abbr_of_yojson json =
  match abbr_of_yojson json with
  | Ok abbr -> abbr
  | Error err -> failwith @@ "Error parsing JSON: " ^ err

let get_success = function
  | `Success obj -> obj
  | _ -> assert_failure "Unexpected failure."


(* api_test *)

let test_api_test_nodata _tctx =
  Slacko.api_test ?base_url () >|= get_success >|= fun json ->
  assert_equal ~printer:Yojson.Safe.to_string
    (`Assoc [])
    json

let test_api_test_foo _tctx =
  Slacko.api_test ?base_url ~foo:"hello" () >|= get_success >|= fun json ->
  assert_equal ~printer:Yojson.Safe.to_string
    (`Assoc ["args", `Assoc ["foo", `String "hello"]])
    json

let test_api_test_err _tctx =
  Slacko.api_test ?base_url ~error:"badthing" () >|= fun resp ->
  assert_equal (`Unhandled_error "badthing") resp

let test_api_test_err_foo _tctx =
  Slacko.api_test ?base_url ~foo:"goodbye" ~error:"badthing" () >|= fun resp ->
  assert_equal (`Unhandled_error "badthing") resp

let api_test_tests = fake_slack_tests "api_test" [
  "test_nodata", test_api_test_nodata;
  "test_foo", test_api_test_foo;
  "test_err", test_api_test_err;
  "test_err_foo", test_api_test_err_foo;
]

(* auth_test *)

let test_auth_test_valid _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.auth_test session >|= get_success >|=
  abbr_authed_obj >|= fun authed ->
  assert_equal ~printer:show_abbr_authed_obj
    (abbr_json abbr_authed_obj_of_yojson Fake_slack.authed_json)
    authed

let test_auth_test_invalid _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.auth_test session >|= fun resp ->
  assert_equal `Invalid_auth resp

let auth_test_tests = fake_slack_tests "test_auth" [
  "test_valid", test_auth_test_valid;
  "test_invalid", test_auth_test_invalid;
]

(* channels_archive  *)

let test_channels_archive_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  let new_channel = Slacko.channel_of_string "#new_channel" in
  Slacko.channels_archive session new_channel >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_channels_archive_existing _tctx =
  let session = Slacko.start_session ?base_url token in
  let new_channel = Slacko.channel_of_string "#archivable_channel" in
  Slacko.channels_archive session new_channel >|= fun resp ->
  assert_equal `Success resp

let test_channels_archive_missing _tctx =
  let session = Slacko.start_session ?base_url token in
  let missing_channel = Slacko.channel_of_string "#missing_channel" in
  Slacko.channels_archive session missing_channel >|= fun resp ->
  assert_equal `Channel_not_found resp

let test_channels_archive_archived _tctx =
  let session = Slacko.start_session ?base_url token in
  let archived_channel = Slacko.channel_of_string "#archived_channel" in
  Slacko.channels_archive session archived_channel >|= fun resp ->
  assert_equal `Already_archived resp

let test_channels_archive_general _tctx =
  let session = Slacko.start_session ?base_url token in
  let general = Slacko.channel_of_string "#general" in
  Slacko.channels_archive session general >|= fun resp ->
  assert_equal `Cant_archive_general resp

let channels_archive_tests = fake_slack_tests "channels_archive" [
  "test_bad_auth", test_channels_archive_bad_auth;
  "test_existing", test_channels_archive_existing;
  "test_missing", test_channels_archive_missing;
  "test_archived", test_channels_archive_archived;
  "test_general", test_channels_archive_general;
]

(* channels_create *)

let test_channels_create_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.channels_create session "#new_channel" >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_channels_create_new _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.channels_create session "#new_channel" >|= get_success >|=
  abbr_channel_obj >|= fun channel ->
  assert_equal ~printer:show_abbr_channel_obj
    (abbr_json abbr_channel_obj_of_yojson Fake_slack.new_channel_json)
    channel

let test_channels_create_existing _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.channels_create session "#general" >|= fun resp ->
  assert_equal `Name_taken resp

let channels_create_tests = fake_slack_tests "channels_create" [
  "test_bad_auth", test_channels_create_bad_auth;
  "test_new", test_channels_create_new;
  "test_existing", test_channels_create_existing;
]

(* channels_history *)

let test_channels_history_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  let new_channel = Slacko.channel_of_string "#new_channel" in
  Slacko.channels_history session new_channel >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_channels_history_no_params _tctx =
  let session = Slacko.start_session ?base_url token in
  let random = Slacko.channel_of_string "#random" in
  Slacko.channels_history session random >|= get_success >|= fun history ->
  assert_equal ~printer:show_abbr_history_obj
    (abbr_json abbr_history_obj_of_yojson Fake_slack.random_history_json)
    (abbr_history_obj history)

let channels_history_tests = fake_slack_tests "channels_history" [
  "test_bad_auth", test_channels_history_bad_auth;
  "test_no_params", test_channels_history_no_params;
]

(* channels_info *)
(* channels_invite *)
(* channels_join *)
(* channels_kick *)
(* channels_leave *)

(* channels_list *)

let test_channels_list_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.channels_list session >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_channels_list _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.channels_list session >|= get_success >|=
  List.map abbr_channel_obj >|= fun channels ->
  assert_equal ~printer:show_abbr_channel_obj_list
    (abbr_json abbr_channel_obj_list_of_yojson Fake_slack.channels_json)
    channels

let channels_list_tests = fake_slack_tests "channels_list" [
  "test_bad_auth", test_channels_list_bad_auth;
  "test", test_channels_list;
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

let test_files_list_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.files_list session >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_files_list _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.files_list session >|= get_success >|=
  abbr_files_list_obj >|= fun files ->
  assert_equal ~printer:show_abbr_files_list_obj
    (abbr_json abbr_files_list_obj_of_yojson Fake_slack.files_json)
    files

let files_list_tests = fake_slack_tests "files_list" [
  "test_bad_auth", test_files_list_bad_auth;
  "test", test_files_list;
]

(* files_upload *)
(* groups_archive *)
(* groups_close *)
(* groups_create *)
(* groups_create_child *)

(* groups_history *)

let test_groups_history_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  let seekrit = Slacko.group_of_string "seekrit" in
  Slacko.groups_history session seekrit >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_groups_history_no_params _tctx =
  let session = Slacko.start_session ?base_url token in
  let seekrit = Slacko.group_of_string "seekrit" in
  Slacko.groups_history session seekrit >|= get_success >|= fun history ->
  assert_equal ~printer:show_abbr_history_obj
    (abbr_json abbr_history_obj_of_yojson Fake_slack.seekrit_history_json)
    (abbr_history_obj history)

let groups_history_tests = fake_slack_tests "groups_history" [
  "test_bad_auth", test_groups_history_bad_auth;
  "test_no_params", test_groups_history_no_params;
]

(* groups_invite *)
(* groups_kick *)
(* groups_leave *)

(* groups_list *)

let test_groups_list_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.groups_list session >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_groups_list _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.groups_list session >|= get_success >|=
  List.map abbr_group_obj >|= fun groups ->
  assert_equal ~printer:show_abbr_group_obj_list
    (abbr_json abbr_group_obj_list_of_yojson Fake_slack.groups_json)
    groups

let groups_list_tests = fake_slack_tests "groups_list" [
  "test_bad_auth", test_groups_list_bad_auth;
  "test", test_groups_list;
]

(* groups_mark *)
(* groups_open *)
(* groups_rename *)
(* groups_set_purpose *)
(* groups_set_topic *)
(* groups_unarchive *)
(* im_close *)

(* im_history *)

let test_im_history_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  let slackbot = Slacko.conversation_of_string Fake_slack.im_slackbot in
  Slacko.im_history session slackbot >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_im_history_no_params _tctx =
  let session = Slacko.start_session ?base_url token in
  let slackbot = Slacko.conversation_of_string Fake_slack.im_slackbot in
  Slacko.im_history session slackbot >|= get_success >|= fun history ->
  assert_equal ~printer:show_abbr_history_obj
    (abbr_json abbr_history_obj_of_yojson Fake_slack.slackbot_history_json)
    (abbr_history_obj history)

let im_history_tests = fake_slack_tests "im_history" [
  "test_bad_auth", test_im_history_bad_auth;
  "test_no_params", test_im_history_no_params;
]

(* im_list *)

let test_im_list_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.im_list session >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_im_list _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.im_list session >|= get_success >|=
  List.map abbr_im_obj >|= fun ims ->
  assert_equal ~printer:show_abbr_im_obj_list
    (abbr_json abbr_im_obj_list_of_yojson Fake_slack.ims_json)
    ims

let im_list_tests = fake_slack_tests "im_list" [
  "test_bad_auth", test_im_list_bad_auth;
  "test", test_im_list;
]

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

let test_users_list_bad_auth _tctx =
  let session = Slacko.start_session ?base_url badtoken in
  Slacko.users_list session >|= fun resp ->
  assert_equal `Invalid_auth resp

let test_users_list _tctx =
  let session = Slacko.start_session ?base_url token in
  Slacko.users_list session >|= get_success >|=
  List.map abbr_user_obj >|= fun users ->
  assert_equal ~printer:show_abbr_user_obj_list
    (abbr_json abbr_user_obj_list_of_yojson Fake_slack.users_json)
    users

let users_list_tests = fake_slack_tests "users_list" [
  "test_bad_auth", test_users_list_bad_auth;
  "test", test_users_list;
]

(* users_set_active *)
(* users_set_presence *)

(* Gotta run them all! *)

let suite = "tests" >::: [
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
    channels_list_tests;
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


let () = run_test_tt_main suite
