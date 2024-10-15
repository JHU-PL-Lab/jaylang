#!/bin/bash

# Input and output directories
in_dir="test/dbmc/concolic/scheme-pldi-2015-bjy"
out_dir="test/dbmc/concolic/scheme-pldi-2015"

# Loop through each file in the input directory
for file in "$in_dir"/*; do
    # Check if the file is a regular file
    if [ -f "$file" ]; then
        # Get the filename without extension
        filename=$(basename "$file")
        filename_no_ext="${filename%.*}"
        
        # Run the translation command
        cat "$file" | ./translator.exe -m bluejay-to-jayil -w > "$out_dir"/"$filename_no_ext".jil
        
        # Create an empty expect file
        touch "$out_dir"/"$filename_no_ext".expect.s
    fi
done
