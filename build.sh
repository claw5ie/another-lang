#!/bin/bash
set -xeu
gcc -Wall -Wextra -pedantic -g src/main.c $@
