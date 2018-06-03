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

(* Wrap Yojson.Safe.json so we don't have to keep providing printers for it. *)
type json = Yojson.Safe.json
[@@deriving yojson]
let pp_json fmt json = Format.pp_print_string fmt (Yojson.Safe.to_string json)

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
  latest: json option [@default None];
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

type abbr_message_obj = {
  type': string [@key "type"];
  ts: timestamp;
  (* user: user; *)
  text: string option;
  is_starred: bool option [@default None];
} [@@deriving show, yojson { strict = false }]

let abbr_message_obj (message : Slacko.message_obj) = {
  type' = message.Slacko.type';
  ts = message.Slacko.ts;
  text = message.Slacko.text;
  is_starred = message.Slacko.is_starred;
}

type abbr_history_obj = {
  latest: timestamp option [@default None];
  messages: abbr_message_obj list;
  has_more: bool;
} [@@deriving show, yojson { strict = false }]

let abbr_history_obj (history : Slacko.history_obj) = {
  latest = history.Slacko.latest;
  messages = List.map abbr_message_obj history.Slacko.messages;
  has_more = history.Slacko.has_more;
}

type abbr_user_obj = {
  (* id: user; *)
  name: string;
  deleted: bool;
  color: string;
  real_name: string;
  tz: string option [@default None];
  tz_label: string;
  tz_offset: int;
  profile: json;
  is_admin: bool;
  is_owner: bool;
  is_primary_owner: bool;
  is_restricted: bool;
  is_ultra_restricted: bool;
  is_bot: bool;
  has_files: bool [@default false];
} [@@deriving show, yojson { strict = false } ]

let abbr_user_obj (user : Slacko.user_obj) = {
  name = user.Slacko.name;
  deleted = user.Slacko.deleted;
  color = user.Slacko.color;
  real_name = user.Slacko.real_name;
  tz = user.Slacko.tz;
  tz_label = user.Slacko.tz_label;
  tz_offset = user.Slacko.tz_offset;
  profile = user.Slacko.profile;
  is_admin = user.Slacko.is_admin;
  is_owner = user.Slacko.is_owner;
  is_primary_owner = user.Slacko.is_primary_owner;
  is_restricted = user.Slacko.is_restricted;
  is_ultra_restricted = user.Slacko.is_ultra_restricted;
  is_bot = user.Slacko.is_bot;
  has_files = user.Slacko.has_files;
}

type abbr_users_list_obj = {
  members: abbr_user_obj list
} [@@deriving of_yojson { strict = false }]

type abbr_user_obj_list = abbr_user_obj list
[@@deriving show]

let abbr_user_obj_list_of_yojson json =
  match abbr_users_list_obj_of_yojson json with
  | Result.Ok obj -> Result.Ok obj.members
  | (Result.Error _) as err -> err


type abbr_file_obj = {
  (* TODO file id type *)
  id: string;
  created: timestamp;
  (* deprecated *)
  timestamp: timestamp;

  name: string option [@default None];
  title: string;
  mimetype: string;
  pretty_type: string;
  (* user: user; *)

  mode: string;
  editable: bool;
  is_external: bool;
  external_type: string;

  size: int;

  (* These two are deprecated and appear to be gone. *)
  (* url: string; *)
  (* url_download: string; *)
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
  (* channels: channel list; *)
  (* groups: group list; *)
  (* ims: conversation list; *)
  initial_comment: json option [@default None];
  num_stars: int option [@default None];
} [@@deriving show, yojson { strict = false }]

let abbr_file_obj (file : Slacko.file_obj) = {
  id = file.Slacko.id;
  created = file.Slacko.created;
  timestamp = file.Slacko.timestamp;
  name = file.Slacko.name;
  title = file.Slacko.title;
  mimetype = file.Slacko.mimetype;
  pretty_type = file.Slacko.pretty_type;
  mode = file.Slacko.mode;
  editable = file.Slacko.editable;
  is_external = file.Slacko.is_external;
  external_type = file.Slacko.external_type;
  size = file.Slacko.size;
  url_private = file.Slacko.url_private;
  url_private_download = file.Slacko.url_private_download;
  thumb_64 = file.Slacko.thumb_64;
  thunb_80 = file.Slacko.thunb_80;
  thumb_360 = file.Slacko.thumb_360;
  thumb_360_gif = file.Slacko.thumb_360_gif;
  thumb_360_w = file.Slacko.thumb_360_w;
  thumb_360_h = file.Slacko.thumb_360_h;
  permalink = file.Slacko.permalink;
  edit_link = file.Slacko.edit_link;
  preview = file.Slacko.preview;
  preview_highlight = file.Slacko.preview_highlight;
  lines = file.Slacko.lines;
  lines_more = file.Slacko.lines_more;
  is_public = file.Slacko.is_public;
  initial_comment = file.Slacko.initial_comment;
  num_stars = file.Slacko.num_stars;
}

type abbr_paging_obj = {
  count: int;
  total: int;
  page: int;
  pages: int;
} [@@deriving show, yojson { strict = false }]

let abbr_paging_obj (paging : Slacko.paging_obj) = {
  count = paging.Slacko.count;
  total = paging.Slacko.total;
  page = paging.Slacko.page;
  pages = paging.Slacko.pages;
}

type abbr_files_list_obj = {
  files: abbr_file_obj list;
  paging: abbr_paging_obj;
} [@@deriving show, yojson { strict = false }]

let abbr_files_list_obj (files : Slacko.files_list_obj) = {
  files = List.map abbr_file_obj files.Slacko.files;
  paging = abbr_paging_obj files.Slacko.paging;
}

type abbr_group_obj = {
  (* id: group; *)
  name: string;
  is_group: bool;
  created: timestamp;
  (* creator: user; *)
  is_archived: bool;
  (* members: user list; *)
  topic: abbr_topic_obj;
  purpose: abbr_topic_obj;
  is_open: bool option [@default None];
  last_read: timestamp option [@default None];
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
  latest: json option [@default None];
} [@@deriving show, yojson { strict = false }]

let abbr_group_obj (group : Slacko.group_obj) = {
  name = group.Slacko.name;
  is_group = group.Slacko.is_group;
  created = group.Slacko.created;
  is_archived = group.Slacko.is_archived;
  topic = abbr_topic_obj group.Slacko.topic;
  purpose = abbr_topic_obj group.Slacko.purpose;
  is_open = group.Slacko.is_open;
  last_read = group.Slacko.last_read;
  unread_count = group.Slacko.unread_count;
  unread_count_display = group.Slacko.unread_count_display;
  latest = group.Slacko.latest;
}

type abbr_group_obj_list = abbr_group_obj list
[@@deriving show, yojson]

type abbr_im_obj = {
  id: string;
  is_im: bool;
  (* user: user; *)
  created: timestamp;
  is_user_deleted: bool;
  unread_count: int option [@default None];
  unread_count_display: int option [@default None];
} [@@deriving show, yojson { strict = false }]

let abbr_im_obj (im : Slacko.im_obj) = {
  id = im.Slacko.id;
  is_im = im.Slacko.is_im;
  created = im.Slacko.created;
  is_user_deleted = im.Slacko.is_user_deleted;
  unread_count = im.Slacko.unread_count;
  unread_count_display = im.Slacko.unread_count_display;
}

type abbr_im_obj_list = abbr_im_obj list
[@@deriving show, yojson]
