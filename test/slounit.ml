let lwt_test test_fun ctx = Lwt_main.run @@ test_fun ctx

let fake_slack_test test_fun ctx =
  lwt_test Fake_slack.with_fake_slack (fun () -> test_fun ctx)

let fake_slack_tests label tests =
  let open OUnit2 in
  label >::: List.map (fun (l, f) -> l >:: fake_slack_test f) tests
