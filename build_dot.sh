#!/bin/bash

ls -1 *.dot | while read line; do
    dot -Tpdf < "$line" > "${line:0:$((${#line}-4))}.pdf"
done

