open Lwt
open OUnit2

open Slounit
open Abbrtypes


let token_str =
  try Sys.getenv "SLACKO_TEST_TOKEN" with Not_found -> "xoxp-testtoken"

let token = Slacko.token_of_string token_str

(* If we have a non-default token, assume we want to talk to real slack. If
   not, use our local fake instead. *)
let () = match token_str with
  | "xoxp-testtoken" -> Slacko.set_base_url "http://127.0.0.1:7357/api/"
  | _ -> print_endline "NOTE: Because an API token has been provided, tests will run against the real slack API."


let abbr_json abbr_of_yojson json =
  match abbr_of_yojson json with
  | Ok abbr -> abbr
  | Error err -> failwith @@ "Error parsing JSON: " ^ err

let get_success = function
  | `Success obj -> obj
  | _ -> assert_failure "Unexpected failure."


(* api_test *)

let test_api_test_nodata tctx =
  Slacko.api_test () >|= get_success >|= fun json ->
  assert_equal ~printer:Yojson.Safe.to_string
    (`Assoc [])
    json

let test_api_test_foo tctx =
  Slacko.api_test ~foo:"hello" () >|= get_success >|= fun json ->
  assert_equal ~printer:Yojson.Safe.to_string
    (`Assoc ["args", `Assoc ["foo", `String "hello"]])
    json

let test_api_test_err tctx =
  Slacko.api_test ~error:"badthing" () >|= fun resp ->
  assert_equal (`Unhandled_error "badthing") resp

let test_api_test_err_foo tctx =
  Slacko.api_test ~foo:"goodbye" ~error:"badthing" () >|= fun resp ->
  assert_equal (`Unhandled_error "badthing") resp

let api_test_tests = fake_slack_tests "api_test" [
  "test_nodata", test_api_test_nodata;
  "test_foo", test_api_test_foo;
  "test_err", test_api_test_err;
  "test_err_foo", test_api_test_err_foo;
]

(* auth_test *)

let test_auth_test_valid tctx =
  Slacko.auth_test token >|= get_success >|=
  abbr_authed_obj >|= fun authed ->
  assert_equal ~printer:show_abbr_authed_obj
    (abbr_json abbr_authed_obj_of_yojson Fake_slack.authed_json)
    authed

let test_auth_test_invalid tctx =
  Slacko.auth_test (Slacko.token_of_string "badtoken") >|= fun resp ->
  assert_equal `Invalid_auth resp

let auth_test_tests = fake_slack_tests "test_auth" [
  "test_valid", test_auth_test_valid;
  "test_invalid", test_auth_test_invalid;
]

(* channels_list *)

let test_channels_list tctx =
  Slacko.channels_list token >|= get_success >|=
  List.map abbr_channel_obj >|= fun channels ->
  assert_equal ~printer:show_abbr_channel_obj_list
    (abbr_json abbr_channel_obj_list_of_yojson Fake_slack.channels_json)
    channels

let channels_list_tests = fake_slack_tests "channels_list" [
  "test", test_channels_list;
]


let suite = "tests" >::: [
    api_test_tests;
    auth_test_tests;
    channels_list_tests;
  ]


let () = run_test_tt_main suite
