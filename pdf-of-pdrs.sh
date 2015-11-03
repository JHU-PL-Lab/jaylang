#!/bin/bash -e

filenames="[ $(
    ls -1 *_PDR_*.pdf |\
    sed -r 's/^(.*)$/"\1"/g' | tr '\n' ','
    ) ]"
pdf_ordering="$(
    python -c '
names='"$filenames"'
def nums_of_filename(f):
  f = f[:-4]
  parts = f.rsplit("_",2)
  return (int(parts[-2]),int(parts[-1]))
names.sort(key=nums_of_filename)
for name in names:
  print name
    '
)"
pdftk $pdf_ordering cat output PDRs.pdf
