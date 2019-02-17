0.15.0
------

* Better handling of timestamps (@paurkedal)
* Update user type to match what Slack returns (@Khady)
* Update OPAM metadata to 2.0 format
* Fixed brittle build

0.14.1
------

* Support additional arguments to `chat.post_message` and `chat.update`
* Adjust build process to use `dune` instead of `jbuilder`
* Replace `topkg` code with `dune-release`

0.14.0
------

* Higher precision timestamps.
* Adjust to changes in Slack APIs.
* Support for CoHTTP > 1.0.
* Ported to jbuilder, should yield easier integration with whole ecosystem,
  faster builds and less boilerplate.
* Depend on OCaml 4.04.
* Add support for releasing via `topkg`, thus adding this changelog.

0.13.0
------

* Fixes a number of previously broken endpoints.
* Adds an integration test to make sure that whatever is implemented keeps on
  working.

0.12.0
------

* Added type `chat` to abstract away from all types of channels Slack supports.
* Removed topic-is-too-long errors as the binding checks the topic length
  beforehand.
* Added `conversation` as a new type instead of string IM conversation type.
* More syntactic sugar in code, endpoint definitions now a long pipe.

0.11.0
------

* The binding now looks up User/Channel/Group IDs and rejects invalid ones.
* The message length is now validated: messages that are too long cannot be
  generated anymore.
* Adds support for some more new Slack methods:
  - `channels.archive`
  - `channels.create`
  - `channels.rename`
  - `channels.unarchive`
  - `groups.archive`
  - `groups.rename`
  - `groups.unarchive`

0.10.0
------

In this release, one of the main criticisms was addressed: the API calls are
now represented by their own types, so there are now channel types, user types
and many more. Some parameters can have only a limited number of values, these
are also represented using their own types so calling the methods with
incorrect values is impossible.

The code was updated to use the new Lwt 2.4.6 ppx macros instead of Camlp4, so
this is the minimum required release. Also, better use OCaml 4.02.0 for
improved support for ppx.

0.9.1
-----

* Added `users.info` method.
* Broken up `apierror` into a set of more relevant types per function.

0.9.0
-----

Time to get this code to the internetz!

Features:

* 100% API coverage and a handy tool to post messages to Slack.
* Also, some rudimentary docs and a neat logo made by yours truly.
