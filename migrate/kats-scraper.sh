#!/bin/bash

set -e

fix_encoding () {
  mv "$1" "$1.tmp"
  iconv -f WINDOWS-1252 -t UTF-8 "$1.tmp" > "$1"
  rm "$1.tmp"
  dos2unix "$1"
}

extract_docid () {
  grep -o "DocID=[0-9]*" <<< "$@" | grep -o "[0-9]*"
}

store_doc () {
  if ! [ -e "$category/$1.html" ] ; then
    if ! wget "$baseurl/show_document_smarty.php?DocID=$1" -O "$category/$1.html"
    then
      echo "document $1 not found on server"
      exit 1
    fi
    fix_encoding "$category/$1.html"
  fi
}

store_pic () {
  fld="$category/$1.pics"
  if ! [ -e "$fld/$2.html" ] ; then
    if ! wget "$baseurl/showpic.php3?PicID=$2" -O "$fld/$2.html"
    then
      rm -f "$fld/$2.html"
      echo "pic $2 not found on server"
      exit 1
    fi
    fix_encoding "$fld/$2.html"
  fi
  # get filename (extension may be caps or not)
  # outlier : http://cms.kats-media.org/public/red/pics/3915_3945.peg
  filename=$(grep -iE "/public/red/pics/[0-9_]*.(jpeg|peg|jpg|tif)" "$fld/$2.html" |\
    head -n 1 | grep -ioE "[0-9_]*.(jpg|jpeg|peg|tif)")
  lname=$(tr '[:upper:]' '[:lower:]' <<< "$filename")
  if ! [ -e "media/$lname" ] ; then
    if ! wget "$baseurl/pics/$filename" -O "media/$lname"
    then
      rm -f "media/$lname"
      echo  "problem with /pics/$filename"
      exit 1
    fi
  fi
  echo "$lname"
}

drop_tags () {
  sed -e 's/<[^>]*>//g' <<< "$1"
  # pandoc -f html -t plain <<< "$1"
  # html2text <<< "$1"
}

scrape () {
  listurl="$1"
  mkdir -p media
  mkdir -p "$category"

  if [ ! -e "$category.html" ] || "$refresh" ; then
    wget "$baseurl/$listurl" -O "$category.html"
    fix_encoding "$category.html"
  fi

  # Introduce linebreaks instead of br tags
  # This splits desciption in title and subtitle

  # Parse list of reports for report metadata and get reports

  sed 's#<[^>]*[Bb][Rr]*>#\n#g' "$category.html" | while read -r line ; do
  # seek show_document
  while ! ( grep "show_document"  <<< "$line" > /dev/null )
  do
    if ! read -r line ; then exit ; fi
  done

  docid="$(extract_docid "$line")"
  meta="$category/$docid.meta"
  md="$category/$docid.md"

  if [ -e "$md" ] ; then continue ; fi
  echo "document: $docid"
  store_doc "$docid"

  {
    echo "---"
    echo "category: $category"
    echo "kats_id: $docid"
    printf "date: "
    drop_tags "$line" | awk -F . '{print $3 "-" $2 "-" $1}'
    read -r line
    printf "place: "
    drop_tags "$line"
  } > "$meta"

  # seek description until end of table row
  # We expect a maximum of two lines
  i=1
  while read -r line && grep -v "</tr>" <<< "$line" > /dev/null
  do
    {
      if (( i == 1 )) ; then printf "title: " ; fi
      if (( i == 2 )) ; then printf "lead: " ; fi
      if (( i > 2 ))  ; then >&2 echo "too long description" ; exit 1 ; fi
      trimmed=$(drop_tags "$line" | xargs)
      if [ "" != "$trimmed" ] ; then (( i++ )) ; fi
      echo "$trimmed"
    } >> "$meta"
    done

  fld="$category/$docid.pics"
  # read pics from the bericht html
  grep -o 'PicID=[0-9]*' "$category/$docid.html" | sed 's#PicID=##' |\
    sort | uniq | while read -r picid ; do
      mkdir -p "$fld"
      fname=$(store_pic "$docid" "$picid")
      {
        printf "description: "
        grep Bildbeschreibung -A 1 "$fld/$picid.html" |\
          tail -1 | sed -e 's/<[^>]*>//g' | sed 's/^[ \t]*//'
        printf "source: "
        grep "Foto:" -A 1 "$fld/$picid.html" |\
          tail -1 | sed -e 's/<[^>]*>//g' | sed 's/^[ \t]*//'
        printf 'file: %s\n' "$fname"
      } > "$fld/$picid.meta"
    done

    if [ -e "$fld" ] ; then
      echo "gallery:" >> "$meta"
      cat "$fld"/*.meta >> "$meta"
    fi

    # convert html tags to utf8
    sed -i 's#$#<br>#' "$meta"
    pandoc -f html -t plain --wrap=preserve -o "$md" "$meta"

    # indent picture metadata and escape yaml
    sed -i 's#^title: #title: |\n  #' "$md"
    sed -i 's#^description:$#  - caption:#' "$md"
    sed -i 's#^description: #  - caption: |\n      #' "$md"
    sed -i 's#^source:$#    source:#' "$md"
    sed -i 's#^source: #    source: |\n      #' "$md"
    sed -i 's#^file:#    filename:#' "$md"

    {
      echo "---"
      echo
    } >> "$md"

    # content from bericht
    {
      # "Pressemitteilung" (most cases)
      # printf "<h3>"
      # grep 'a name="1"' "$category/$docid.html" |\
      #   sed -e 's/<[^>]*>//g'
      # echo "</h3>"
      echo "<p>"
      grep 'a name="1"' -A 1 "$category/$docid.html" |\
        tail -n 1 | sed 's#<[^>]*[Bb][Rr]*>#</p>\n<p>#g'
      echo "</p>"
    } | pandoc -f html -t markdown_strict --wrap=none >> "$md"

  done
}

baseurl="http://cms.kats-media.org/public/red"

if [ "$1" = "refresh" ]
then
  refresh=true
else
  refresh=false
fi

workdir="$(git rev-parse --show-toplevel)/migrate/_kats"
mkdir -p "$workdir"
cd "$workdir"

category=einsaetze
scrape "uebersicht_pm_smarty.php/?Category=1001100"
category=neues
scrape "uebersicht_pm_smarty.php/?Category=1001200"
