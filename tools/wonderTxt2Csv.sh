#!/bin/bash

FILE=$1

FILE_NEW=$(echo $FILE | sed 's/.txt//g' | sed 's/ /_/g' | sed 's/-/_/g'| sed 's/,//g')

cp "$FILE" "$FILE_NEW.csv"

sed -i -e 's/	/,/g' "$FILE_NEW.csv" # Replace Tabs with comma

rm "$FILE_NEW.csv-e"
