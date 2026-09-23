#!/bin/sh

# immediately when a command fails and print each command
set -ex

sudo chown -R opam: _build

opam init -a --shell=zsh

opam remote remove --all default
opam repository add default --all-switches --set-default https://opam.ocaml.org

opam install --with-test --with-doc --deps-only -y .

make deps
