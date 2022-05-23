#!/usr/bin/env bash

cargo run -- "$1" "${1}.c"
gcc -o "${1}.exe" "${1}.c"
