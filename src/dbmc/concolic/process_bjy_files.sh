#!/bin/bash

# This script is to be run from jaylang/

input_dir="./test/sato/bjy/"
output_dir="./test/dbmc/concolic/_bjy_tests/"
translator_exe="./_build/default/src/bin/translator.exe"

# Create translator exe
make translator

# Check if translator executable exists where we expect it
if [ ! -f "${translator_exe}" ]; then
    echo "Error: Translator executable not found at expected directory after 'make translator'."
    exit 1
fi

overwrite=false
while getopts ":o" opt; do
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
    (cat "${bjy_file}") | "${translator_exe}" -m bluejay-to-jayil > "${output_dir}${file_name}.jil"

    # Check if .expect.s file exists, then copy it
    expect_s_file="${input_dir}${file_name}.expect.s"
    if [ -f "${expect_s_file}" ]; then
        cp "${expect_s_file}" "${output_dir}"
    fi

    # delete tildes that are put in front of some variables
    # This is not needed after we have added `purge` to the translator
    # sed -i -E 's/(~)([^[:space:]]+)/\2/g' "${jil_file}"

done
