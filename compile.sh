#!/usr/bin/env bash

cargo run -- "$1" "${1}.c"
gcc -o "${1}.exe" -g -fsanitize=address -lasan "${1}.c"
