#!/usr/bin/env bash

set -e

cargo run -- "$1" "${1}.c"
gcc -o "${1}.exe" -g -fsanitize=address -lasan "${1}.c"
