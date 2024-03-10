#!/bin/bash

input_dir="./benchmark/concolic/bjy/"
output_dir="./benchmark/concolic/jil/"
translator_exe="./translator.exe"

# Create translator exe
make translator

# Assert that translator executable exists where we expect it
if [ ! -f "${translator_exe}" ]; then
    echo "Error: Translator executable not found at expected directory after 'make translator'."
    exit 1
fi

overwrite=false
while getopts ":o:" opt; do
  case ${opt} in
    o )
      overwrite=true
      ;;
    \? )
      echo "Invalid option: -$OPTARG"
      exit 1
      ;;
    : )
      echo "Option -$OPTARG requires an argument."
      exit 1
      ;;
  esac
done

for bjy_file in "${input_dir}"*.bjy; do
    # Extracting file name without extension
    file_name=$(basename "${bjy_file%.bjy}")

    # Check if .jil file exists
    jil_file="${output_dir}${file_name}.jil"
    if [ -f "${jil_file}" ] && [ "${overwrite}" = false ]; then
        echo "Skipped: ${jil_file} already exists. Use -o flag to overwrite."
        continue
    fi

    # Run the translator and save output to .jil file with the same name
    (cat "${bjy_file}") | "${translator_exe}" -m bluejay-to-jayil -a -w > "${output_dir}${file_name}.jil"
done