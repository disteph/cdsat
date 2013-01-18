#!/bin/sh
for file in $(ls aim*.cnf); do
problemname="$(echo "$file" | cut -d"." -f1)"
outputname="$(echo "$file" | cut -d"." -f1).out"
nbvar="$(grep -w 'p cnf' "$file" | cut -d " " -f2| cut -d " " -f3)"
nbclauses="$(grep -w 'p cnf' "$file" | cut -d " " -f2| cut -d " " -f4)"
successtime="$(grep seconds "$outputname")"
error="$(grep error "$outputname")"
line="$problemname"" | ""$nbvar"" | ""$nbclauses"" | ""$successtime"" | ""$error"
echo "$line"
done