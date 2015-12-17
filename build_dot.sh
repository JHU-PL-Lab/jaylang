#!/bin/bash

ls -1 *.dot | while read line; do
    if [ "$1" = "-v" ]; then
        echo "$line"
    fi
    dot -Tpdf < "$line" > "${line:0:$((${#line}-4))}.pdf"
done

