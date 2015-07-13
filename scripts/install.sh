#!/bin/bash  

# this script is designed to set up coracle on an ubuntu VM

# get opam setup
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get upgrade
sudo apt-get install ocaml ocaml-native-compilers camlp4-extra opam make m4
opam init
eval `opam config env`

# setup ocaml compiler and libaries for coracle
opam switch 4.02.2
eval `opam config env`
opam update
opam upgrade
opam install cstruct ipaddr lwt sexplib cmdliner

# get code
./scripts/update.sh