let api_test () =
  let uri = Uri.of_string "https://slack.com/api/api.test" in
  lwt (response, body) = Cohttp_lwt_unix.Client.get uri in
  Cohttp_lwt_body.to_string body
