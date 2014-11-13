OPAM_DEPENDS="lwt>=2.4.6 ssl cmdliner cohttp>=0.10.0 yojson"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
4.02.1,1.1.0) ppa=avsm/ocaml42+opam11 ;;
4.02.1,1.2.0) ppa=avsm/ocaml42+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

sudo add-apt-repository --yes ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time

export OPAMYES=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam update
sudo apt-get install -qq `opam install -e ubuntu ${OPAM_DEPENDS}`
opam install ${OPAM_DEPENDS}
opam install oasis
eval `opam config env`

oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
sudo ocaml setup.ml -install
sudo ocaml setup.ml -uninstall
