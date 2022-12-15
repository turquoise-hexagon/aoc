#!/usr/bin/env bash

./convert.sh < input > input.dzn

minizinc -p 8 --solver or-tools solution.mzn input.dzn
