(*
* slack-notify - Posts messages to Slack channels
* Copyright (C) 2014-2015 Marek Kubica <marek@xivilization.net>
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

let base_url =
  let doc = "The Slack API base URL" in
  Cmdliner.Arg.(value & opt (some string) None & info ["base-url"] ~docv:"URL" ~doc)

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let username =
  let doc = "Name of the bot in the chat" in
  Cmdliner.Arg.(value & opt (some string) None & info ["u"; "username"] ~docv:"USERNAME" ~doc)

let icon_url =
  let doc = "URL to an image to use as the icon for this message" in
  Cmdliner.Arg.(value & opt (some string) None & info ["icon-url"] ~docv:"URL" ~doc)

let icon_emoji =
  let doc = "Emoji to use as the icon for this message. Overrides icon-url" in
  Cmdliner.Arg.(value & opt (some string) None & info ["icon-emoji"] ~docv:"EMOJI" ~doc)

let channel =
  let doc = "Name of the channel to post to" in
  Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"CHANNEL" ~doc)

let message =
  let doc = "Message to send" in
  Cmdliner.Arg.(required & pos 1 (some string) None & info [] ~docv:"MSG" ~doc)

let attachment =
  let doc = "Attachment text" in
  Cmdliner.Arg.(value & opt (some string) None & info ["attachment"] ~docv:"MSG" ~doc)

let info =
  let doc = "Posts messages to Slack" in
  Cmdliner.Term.info "slack-notify" ~doc

let execute base_url token username channel icon_url icon_emoji attachment_text msg =
  "Your token is " ^ token ^ ", the channel is " ^ channel
    ^ " and the message is '" ^ msg ^ "'."
    |> print_endline;

  let string_or_bust = function
    | `Success _ -> "Message posted"
    | `Invalid_auth -> "Invalid token"
    | `Channel_not_found -> "Channel unknown"
    | `Is_archived -> "Channel is archived"
    | `Msg_too_long -> "Message too long"
    | `Rate_limited -> "Rate limit active"
    | _ -> "Unknown error"
  in

  let open Lwt in
  let session = Slacko.start_session ?base_url token in
  let channel = Slacko.channel_of_string channel in
  let chat = Slacko.Channel channel in
  let msg = Slacko.message_of_string msg in
  let attachments =
    match attachment_text with
    | None -> None
    | Some text -> Some [Slacko.attachment ~text ()]
  in
  Slacko.chat_post_message session chat
    ?username:(username)
    ?icon_emoji:(icon_emoji)
    ?icon_url:(icon_url)
    ?attachments:(attachments)
    msg
  >|= (fun c ->
    print_endline @@ string_or_bust c)
  |> Lwt_main.run

let execute_t = Cmdliner.Term.(
    pure execute $ base_url $ token $ username $ channel $ icon_url $ icon_emoji
    $ attachment $ message)

let () =
  match Cmdliner.Term.eval (execute_t, info) with
    | `Error _ -> exit 1
    | _ -> exit 0
