#!/bin/bash -e

pdfs="$(
    ls -1 *_PDR_*.pdf |\
    sed -r 's/(.*)_PDR_([0-9]+)_([0-9]+).pdf/\2_\3___@@\1_PDR_\2_\3.pdf/g' |\
    sort -n |\
    sed -r 's/^.*___@@//g'
)"
pdftk $pdfs cat output PDRs.pdf
