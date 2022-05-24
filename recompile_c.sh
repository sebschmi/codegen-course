#!/usr/bin/env bash

gcc -o "${1}.exe" -g -fsanitize=address -lasan "${1}.c"
