#!/bin/sh
rm -rf dubois.txt
touch dubois.txt
(for file in $(ls dubois*.cnf); do
problemname="$(echo "$file" | cut -d"." -f1)"
outputname="$(echo "$file" | cut -d"." -f1).out"
nbvar="$(grep -w '^p cnf' "$file" | cut -d " " -f3)"
nbclauses="$(grep -w '^p cnf' "$file" | cut -d " " -f4)"
successtime="$(grep Total "$outputname")"
error="$(grep error "$outputname")"
line="$problemname"" | ""$nbvar"" | ""$nbclauses"" | ""$successtime"" | ""$error"
echo "$line"
done)> dubois.txt