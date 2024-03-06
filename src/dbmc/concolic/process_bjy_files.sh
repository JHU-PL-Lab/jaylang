#!/bin/bash

# This script is to be run from jaylang/

input_dir="./test/sato/bjy/"
output_dir="./test/dbmc/concolic/bjy_tests/"
_input_dir="./test/sato/_bjy/"
_output_dir="./test/dbmc/concolic/_bjy_tests/"
translator_exe="./_build/default/src/bin/translator.exe"

# Create translator exe
make translator

# Assert that translator executable exists where we expect it
if [ ! -f "${translator_exe}" ]; then
    echo "Error: Translator executable not found at expected directory after 'make translator'."
    exit 1
fi

overwrite=false
include_underscore=false # flag to include the tests in _bjy
single_file_mode=false
while getopts ":ouf:" opt; do
  case ${opt} in
    o )
      overwrite=true
      ;;
    u )
      include_underscore=true
      ;;
    f )
      single_file_mode=true
      file_to_translate="$input_dir$OPTARG"
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

if [ "$single_file_mode" = true ]; then
    # Extracting file name without extension
    file_name=$(basename "${file_to_translate%.bjy}")

    # Check if .jil file exists
    jil_file="${output_dir}${file_name}.jil"
    if [ -f "${jil_file}" ] && [ "${overwrite}" = false ]; then
        echo "Skipped: ${jil_file} already exists. Use -o flag to overwrite."
        exit 0
    fi

    # Run the translator and save output to .jil file with the same name
    (cat "${file_to_translate}") | "${translator_exe}" -m bluejay-to-jayil -a > "${output_dir}${file_name}.jil"

    # Check if .expect.s file exists, then copy it
    expect_s_file="${input_dir}${file_name}.expect.s"
    if [ -f "${expect_s_file}" ]; then
        cp "${expect_s_file}" "${output_dir}"
    fi

    # delete tildes that are put in front of some variables
    # This is not needed after we have added `purge` to the translator
    # sed -i -E 's/(~)([^[:space:]]+)/\2/g' "${jil_file}"
else
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
        (cat "${bjy_file}") | "${translator_exe}" -m bluejay-to-jayil -a > "${output_dir}${file_name}.jil"

        # Check if .expect.s file exists, then copy it
        expect_s_file="${input_dir}${file_name}.expect.s"
        if [ -f "${expect_s_file}" ]; then
            cp "${expect_s_file}" "${output_dir}"
        fi

      # delete tildes that are put in front of some variables
      # This is not needed after we have added `purge` to the translator
      # sed -i -E 's/(~)([^[:space:]]+)/\2/g' "${jil_file}"
    done

    if [ "$include_underscore" = true ]; then
        for bjy_file in "${_input_dir}"*.bjy; do
            # Extracting file name without extension
            file_name=$(basename "${bjy_file%.bjy}")

            # Check if .jil file exists
            jil_file="${_output_dir}${file_name}.jil"
            if [ -f "${jil_file}" ] && [ "${overwrite}" = false ]; then
                echo "Skipped: ${jil_file} already exists. Use -o flag to overwrite."
                continue
            fi

            # Run the translator and save output to .jil file with the same name
            (cat "${bjy_file}") | "${translator_exe}" -m bluejay-to-jayil -a > "${_output_dir}${file_name}.jil"

            # Check if .expect.s or .ss file exists, then copy it to .s
            # (incomplete error messages are in .ss files)
            expect_s_file="${_input_dir}${file_name}.expect.s"
            if [ -f "${expect_s_file}" ]; then
                cp "${expect_s_file}" "${_output_dir}"
            fi

            ss_file="${_input_dir}${file_name}.ss"
            if [ -f "${ss_file}" ]; then
                cp "${ss_file}" "${_output_dir}${file_name}.s"
            fi
        done

    fi
fi
