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

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let channel =
  let doc = "Name of the channel to post to" in
  Cmdliner.Arg.(required & opt (some string) None & info ["c"; "channel"] ~docv:"CHANNEL" ~doc)

let info =
  let doc = "Writes messages to slack" in
  Cmdliner.Term.info "slack-notify" ~doc

let execute token channel =
  "Your token is " ^ token ^ "."
  |> print_endline;
  print_endline channel;

  let string_or_bust = function
    | `Success json -> Yojson.Basic.pretty_to_string json
    | `Invalid_auth -> "Invalid token"
    | `Channel_not_found -> "Channel unknown"
    | `Is_archived -> "Channel is archived"
    | `Msg_too_long -> "Message too long"
    | `Rate_limited -> "Rate limit active"
    | _ -> "Unknown error"
  in

  let open Lwt in
  Slacko.chat_post_message token "#geloetnotexist" "Test bot"
  >>= (fun c ->
    return (print_endline @@ string_or_bust c))
  (*
  Slacko.api_test ~foo:"whatever" () >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.auth_test token >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.channels_list token
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.channels_history token "C02LFT2FJ"
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.channels_info token "C02LFT2FJ"
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.emoji_list token
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.im_list token
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.stars_list token
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.users_list token
  >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.users_set_active token
  >>= (fun c ->
    return (print_endline @@ string_or_bust c))
  *)
  |> Lwt_main.run

let execute_t = Cmdliner.Term.(pure execute $ token $ channel)

let () =
  match Cmdliner.Term.eval (execute_t, info) with
    | `Error _ -> exit 1
    | _ -> exit 0
