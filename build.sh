#!/bin/bash
set -xeu
gcc -Wall -Wextra -pedantic src/main.c $@
