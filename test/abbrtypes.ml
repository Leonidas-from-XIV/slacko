(* Slacko's public interface doesn't let us easily construct Slacko.user and
   Slacko.channel values from JSON, and it doesn't let us extract strings from
   those values at all. Therefore, we copy the record type that use these and
   skip the problematic fields. *)

(* These are copied directly from slacko.ml so we can use them here. *)
type timestamp = float
let timestamp_to_yojson ts = `Int (int_of_float ts)
let timestamp_of_yojson = function
  | `Int x -> Result.Ok (float_of_int x)
  | `Intlit x -> Result.Ok (float_of_string x)
  | `String x -> Result.Ok (float_of_string x)
  | _ -> Result.Error "Couldn't parse timestamp type"
(* But this one is new. *)
let pp_timestamp fmt ts = Format.pp_print_float fmt ts

type abbr_authed_obj = {
  url: string;
  team: string;
  user: string;
  team_id: string;
  (* user_id: user; *)
} [@@deriving make, show, yojson { strict = false }]

let abbr_authed_obj (authed : Slacko.authed_obj) = {
  url = authed.Slacko.url;
  team = authed.Slacko.team;
  user = authed.Slacko.user;
  team_id = authed.Slacko.team_id;
}

type abbr_topic_obj = {
  value: string;
  (* creator: user; *)
  last_set: timestamp;
} [@@deriving show, yojson { strict = false }]

let abbr_topic_obj (topic : Slacko.topic_obj) = {
  value = topic.Slacko.value;
  last_set = topic.Slacko.last_set;
}

let opt_json_to_string = function
  | Some json -> Yojson.Safe.to_string json
  | None -> Yojson.Safe.to_string `Null

type abbr_channel_obj = {
  (* id: channel; *)
  name: string;
  is_channel: bool;
  created: timestamp;
  (* creator: user; *)
  is_archived: bool;
  is_general: bool;
  is_member: bool;
  (* members: user list; *)
  topic: abbr_topic_obj;
  purpose: abbr_topic_obj;
  last_read: timestamp option [@default None];
  latest: Yojson.Safe.json option [@default None]
      [@printer fun fmt v -> fprintf fmt "%s" (opt_json_to_string v)];
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
  num_members: int option [@default None];
} [@@deriving show, yojson { strict = false }]

let abbr_channel_obj (chan : Slacko.channel_obj) = {
  name = chan.Slacko.name;
  is_channel = chan.Slacko.is_channel;
  created = chan.Slacko.created;
  is_archived = chan.Slacko.is_archived;
  is_general = chan.Slacko.is_general;
  is_member = chan.Slacko.is_member;
  topic = abbr_topic_obj chan.Slacko.topic;
  purpose = abbr_topic_obj chan.Slacko.purpose;
  last_read = chan.Slacko.last_read;
  latest = chan.Slacko.latest;
  unread_count = chan.Slacko.unread_count;
  unread_count_display = chan.Slacko.unread_count_display;
  num_members = chan.Slacko.num_members;
}

type abbr_channel_obj_list = abbr_channel_obj list
[@@deriving show, yojson]
