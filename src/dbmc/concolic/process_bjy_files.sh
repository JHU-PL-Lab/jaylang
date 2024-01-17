#!/bin/bash

# This script is to be run from jaylang/

input_dir="./test/sato/bjy/"
output_dir="./test/dbmc/concolic/bjy_tests/"
translator_exe="./_build/default/src/bin/translator.exe"

# Create translator exe
make translator

# Check if translator executable exists where we expect it
if [ ! -f "${translator_exe}" ]; then
    echo "Error: Translator executable not found at expected directory after 'make translator'."
    exit 1
fi

for bjy_file in "${input_dir}"*.bjy; do
    # Extracting file name without extension
    file_name=$(basename "${bjy_file%.bjy}")

    # Run the translator and save output to .jil file with the same name
    (cat "${bjy_file}") | "${translator_exe}" -m bluejay-to-jayil > "${output_dir}${file_name}.jil"

    # Check if .expect.s file exists, then copy it
    expect_s_file="${input_dir}${file_name}.expect.s"
    if [ -f "${expect_s_file}" ]; then
        cp "${expect_s_file}" "${output_dir}"
    fi
done
