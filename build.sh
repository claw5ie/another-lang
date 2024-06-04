#!/bin/bash
set -xeu
g++ -Wall -Wextra -pedantic -g -std=c++20 -o compiler src/main.cpp $@
