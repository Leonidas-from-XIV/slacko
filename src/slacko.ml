let api_test ?foo () =
  let base = Uri.of_string "https://slack.com/api/api.test" in
  let uri = match foo with
    | None -> base
    | Some value -> Uri.add_query_param' base ("foo", value) in
  lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
  Cohttp_lwt_body.to_string body

let auth_test token =
  let base = Uri.of_string "https://slack.com/api/auth.test" in
  let uri = Uri.add_query_param' base ("token", token) in
  lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
  Cohttp_lwt_body.to_string body
