(cd lib/yalce; make libyalce_synth.so && sudo make install)
echo "building ocaml lib:"
dune build
