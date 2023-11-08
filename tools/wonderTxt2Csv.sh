#!/bin/bash

FILE=$1

FILE_NEW=$(
  echo $FILE | sed 's/.txt//g' | sed 's/ /_/g' | sed 's/-/_/g' | sed 's/,//g'
)

cp "$FILE" "$FILE_NEW.csv"

# Replace Tabs with comma, remove comments
sed -i -e 's/	/,/g;/---/,$d' "$FILE_NEW.csv"

if [ "$(uname)" == "Darwin" ]; then
  rm "$FILE_NEW.csv-e"
fi
