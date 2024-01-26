(lang dune 2.0)
(name slacko)

(generate_opam_files true)

(authors "Marek Kubica <marek@xivilization.net>")
(maintainers "Marek Kubica <marek@xivilization.net>")
(license "OCaml-LGPL-3.0-linking-exception")
(source (github Leonidas-from-XIV/slacko))

(package
  (name slacko)
  (synopsis "Type-safe binding to the Slack API")
  (description "Slacko provides an easy to use interface to the Slack REST API, which allows to
join Slack channels, post messages, create channels and groups and manage
those, upload and search files, manage presence.")
  (depends
    (ocaml (>= 4.08))
    (cmdliner (>= 1.1.0))
    (yojson (>= 1.6.0))
    (lwt (>= 5.3.0))
    tls-lwt
    (cohttp-lwt-unix (>= 1.0.0))
    (ppx_deriving_yojson (>= 3.3))
    ptime
    (alcotest :with-test)
    (alcotest-lwt :with-test)
    (ppx_deriving (and :with-test (>= 5.2.1))))
  (conflicts
    ;; broken release: https://github.com/mirage/ocaml-conduit/issues/189
    (conduit (= 0.14.1))))
