let base_url = "https://slack.com/api/"

let endpoint e =
  base_url ^ e
  |> Uri.of_string

let api_test ?foo () =
  let base = endpoint "api.test" in
  let uri = match foo with
    | None -> base
    | Some value -> Uri.add_query_param' base ("foo", value) in
  lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
  Cohttp_lwt_body.to_string body

let auth_test token =
  let base = endpoint "auth.test" in
  let uri = Uri.add_query_param' base ("token", token) in
  lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
  Cohttp_lwt_body.to_string body
