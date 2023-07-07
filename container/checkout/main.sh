#!/usr/bin/env bash

set -e

if [ ! -e "/website.git/refs/heads/$BRANCH" ] ; then
  echo "/website.git/refs/heads/$BRANCH does not exist; abort" >&2
  exit 1
fi

echo "/website.git/refs/heads/$BRANCH" | entr -n bash /script/checkout.sh
