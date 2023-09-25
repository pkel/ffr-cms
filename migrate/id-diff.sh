#!/bin/bash

(cd _website && git status)

# which IDs are imported?
kats_ids_in_cms () {
  grep 'kats_id:' -r _website/content |\
    awk '{print $2}' |
    sort -un
}

# which IDs are in kats?
kats_ids_in_kats () {
  grep 'DocID=[0-9]*' -o migrate/_kats/neues.html migrate/_kats/einsaetze.html |\
    sed 's/DocID=\([0-9]*\)/DocID \1/' |\
    awk '{print $2}' |\
    sort -un
}

diff_ids () {
  diff <(kats_ids_in_cms) <(kats_ids_in_kats)
}

echo
echo "Kats IDs in CMS but not in Kats:"

while read -r id ; do
  echo -n "$id "
  grep "kats_id: $id" _website -r --files-with-matches
done < <(diff_ids | grep '^<' | awk '{print $2}')

echo
echo "Kats IDs in Kats but not in CMS:"
while read -r id ; do
  echo "$id http://cms.kats-media.org/public/red/show_document.php3?DocID=$id&MandantID=1&ONLY=0"
done < <(diff_ids | grep '^>' | awk '{print $2}')
