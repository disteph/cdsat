#!/bin/sh
for file in $(ls test_*.sh); do
    ./$file
done
for file in $(ls survey_*.sh); do
    ./$file
done