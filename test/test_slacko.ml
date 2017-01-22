open OUnit2
open Slounit
open Lwt


let token =
  Slacko.token_of_string @@
  try Sys.getenv "SLACKO_TEST_TOKEN" with Not_found -> "token"

let deopt default = function
  | Some x -> x
  | None -> default

(* Slacko's public interface doesn't let us easily construct Slacko.user and
   Slacko.channel values from JSON, and it doesn't let us extract strings from
   those values at all. Therefore, we copy the record type that use these and
   skip the problematic fields. *)
type abbreviated_topic_obj = {
  value: string;
  (* creator: user; *)
  last_set: (* timestamp *) float;
} [@@deriving show, yojson { strict = false }]

type abbreviated_channel_obj = {
  (* id: channel; *)
  name: string;
  is_channel: bool;
  created: (* timestamp *) float;
  (* creator: user; *)
  is_archived: bool;
  is_general: bool;
  is_member: bool;
  (* members: user list; *)
  topic: abbreviated_topic_obj;
  purpose: abbreviated_topic_obj;
  last_read: (* timestamp *) float option [@default None];
  latest: Yojson.Safe.json option [@default None] [@printer fun fmt v -> fprintf fmt "%s" (deopt `Null v |> Yojson.Safe.to_string)];
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
  num_members: int option [@default None];
} [@@deriving show, yojson { strict = false }]

type abbreviated_channel_obj_list = abbreviated_channel_obj list
[@@deriving show, yojson]

let channels_obj =
  match abbreviated_channel_obj_list_of_yojson Fake_slack.channels_json with
  | Ok chans -> chans
  | Error err -> failwith @@ "Invalid channel list: " ^ err


let abbreviate_topic_obj (topic : Slacko.topic_obj) = {
  value = topic.Slacko.value;
  last_set = topic.Slacko.last_set;
}

let abbreviate_channel_obj (chan : Slacko.channel_obj) = {
  name = chan.Slacko.name;
  is_channel = chan.Slacko.is_channel;
  created = chan.Slacko.created;
  is_archived = chan.Slacko.is_archived;
  is_general = chan.Slacko.is_general;
  is_member = chan.Slacko.is_member;
  topic = abbreviate_topic_obj chan.Slacko.topic;
  purpose = abbreviate_topic_obj chan.Slacko.purpose;
  last_read = chan.Slacko.last_read;
  latest = chan.Slacko.latest;
  unread_count = chan.Slacko.unread_count;
  unread_count_display = chan.Slacko.unread_count_display;
  num_members = chan.Slacko.num_members;
}

let get_success = function
  | `Success obj -> obj
  | _ -> assert_failure "Unexpected failure."


let test_channels_list tctx =
  Slacko.channels_list token >|= get_success >|=
  List.map abbreviate_channel_obj >|= fun channels ->
  assert_equal ~printer:show_abbreviated_channel_obj_list channels channels_obj


let suite = "tests" >::: [
    "test_channels_list" >:: fake_slack_test test_channels_list;
  ]


let () = run_test_tt_main suite
