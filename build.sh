(cd lib/yalce; make build/libyalce_synth.so && sudo make install)
echo "building ocaml lib:"
dune build
