#!/bin/sh

while IFS= read -r pkg; do
  libs="${libs} ${pkg}"
done <"dependencies.txt"

install.r "$libs"
