#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
  while IFS= read -r pkg; do
    Rscript -e "install.packages('${pkg}', Ncpus=9)"
  done <"dependencies.txt"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  while IFS= read -r pkg; do
    libs="${libs} ${pkg}"
  done <"dependencies.txt"
  install.r "$libs"
fi
