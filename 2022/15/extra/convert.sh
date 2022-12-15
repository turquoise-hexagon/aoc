#!/usr/bin/env bash

mem=()

while IFS= read -r line; do
    read -r a b c d <<< ${line//[^0-9 -]}

    mem+=("|$a, $b, $c, $d")
done

echo "length = ${#mem[@]};"
echo "input =";

flag=1

for i in "${mem[@]}"; do
    ((flag))          \
        && echo "[$i" \
        || echo " $i"

    flag=0
done

echo " |];"
