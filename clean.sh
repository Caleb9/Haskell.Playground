#!/bin/bash

echo "# Removing following files:"

find -type f -regextype posix-egrep -regex "\.(/\w+)+(\.(hi|o))?|\..+~" -delete -print

echo "# Done"
