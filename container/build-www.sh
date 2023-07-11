#!/usr/bin/env bash
#
# to be used together with ./watch.sh

set -Eeuxo pipefail

mkdir -p "$HOME/checkout"
cd "$HOME/checkout"
if [ -d ".git" ] ; then
  git fetch
else
  rm -rf -- ..?* .[!.]* *
  git clone /website.git ./
fi
git checkout -f "$(cat "/website.git/refs/heads/$BRANCH")"
git submodule update --init --recursive
mkdir -p /www
hugo --destination /www
