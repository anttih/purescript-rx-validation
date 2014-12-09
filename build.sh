#!/bin/bash

set -e

if [ "$#" -eq 0 ]; then cmd=help; else cmd=$1; fi

purs_files () {
  find -L bower_components -name '*.purs' -path '*/src/*' ! -path '*/bower_components/*'
  find src -name '*.purs'
}

case "$cmd" in
  psc-make)
    purs_files | xargs psc-make
  ;;
  psc-docs)
    psc-docs src/Rx/Validation.purs > README.md
  ;;
  dot-psci)
    purs_files | sed 's/^/:m /' > .psci
  ;;
  help)
    cat >&2 <<USAGE
Usage: ./build.sh <task>

Available tasks:

    psc-make         Compile .purs sources to CommonJS modules
    psc-docs         Write docs to README.md
    dot-psci         Add modules to .psci
    help             Show usage
USAGE
esac
