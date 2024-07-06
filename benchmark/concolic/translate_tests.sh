#!/bin/bash

input_dir="./benchmark/concolic/bjy/"
output_dir="./benchmark/concolic/jil_wrap/"
output_dir_no_wrap="./benchmark/concolic/jil_no_wrap/"
translator_exe="./translator.exe"

# Function to handle operations related to output directories
process_output_dir() {
    local dir="$1"
    local file_name="$2"
    local overwrite="$3"
    local bjy_file="$4"
    local wrap="$5"  # New parameter to pass wrap flag

    local jil_file="${dir}${file_name}.jil"

    if [ -f "${jil_file}" ] && [ "${overwrite}" = false ]; then
        echo "Skipped: ${jil_file} already exists in ${dir}. Use -o flag to overwrite."
        return 1
    fi

    # Construct command to run translator with or without -w flag
    local translator_command="${translator_exe} -m bluejay-to-jayil -a"
    if [ "$wrap" = true ]; then
        translator_command+=" -w"
    fi

    # Run translator command and save output to .jil file with the same name
    (cat "${bjy_file}") | ${translator_command} > "${jil_file}"
    echo "Translated: ${bjy_file} to ${jil_file} in ${dir}"
}

# Create translator exe
make translator

# Assert that translator executable exists where we expect it
if [ ! -f "${translator_exe}" ]; then
    echo "Error: Translator executable not found at expected directory after 'make translator'."
    exit 1
fi

# Default values
overwrite=false
wrap=false
custom_input_dir=false

while getopts "oi:w" opt; do
    case ${opt} in
        o )
            overwrite=true
            ;;
        i )
            input_dir="./benchmark/concolic/${OPTARG}/"
            custom_input_dir=true
            ;;
        w )
            wrap=true
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

# Determine the output directory based on the wrap flag
if [ "$wrap" = true ]; then
    output_dir_target="$output_dir"
else
    output_dir_target="$output_dir_no_wrap"
fi

if [ "$custom_input_dir" = false ]; then
    echo "Using default input directory: $input_dir"
fi

for bjy_file in "${input_dir}"*.bjy; do
    # Extracting file name without extension
    file_name=$(basename "${bjy_file%.bjy}")

    # Process files for the selected output directory
    process_output_dir "${output_dir_target}" "${file_name}" "${overwrite}" "${bjy_file}" "$wrap"
done
