#!/bin/sh
for file in $(ls ssa*-*.cnf); do
    echo "$file"
    outputname="$(echo "$file" | cut -d"." -f1).out"
    ../../main.native "$file">"$outputname" 2>&1
done