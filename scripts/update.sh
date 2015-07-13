#!/bin/bash  

# this script is designed to pull the lastest coracle code from github and recompile
# assume that update.sh is ran from project root directory

# clean up
ocaml setup.ml -clean

# fetch latest code & libraries
opam update
opam upgrade
git pull

# recompile
ocaml setup.ml -configure
ocaml setup.ml -build
ocaml setup.ml -install