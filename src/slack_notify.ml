let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(value & opt string "" & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let info =
  let doc = "Writes messages to slack" in
  Cmdliner.Term.info "slack-notify" ~doc

let execute token =
  "Your token is " ^ token ^ "."
  |> print_endline;

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
  Slacko.api_test ~foo:"whatever" () >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.auth_test token >>= (fun c ->
    return (print_endline @@ string_or_bust c)) >>
  Slacko.chat_post_message token "#geloetnotexist" "Test bot"
  >>= (fun c ->
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
    return (print_endline @@ string_or_bust c))
  |> Lwt_main.run

let execute_t = Cmdliner.Term.(pure execute $ token)

let () =
  match Cmdliner.Term.eval (execute_t, info) with
    | `Error _ -> exit 1
    | _ -> exit 0
