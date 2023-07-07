#!/usr/bin/env bash

set -xe

cd /checkout
if [ -d ".git" ] ; then
  git fetch
  git reset --hard "origin/$BRANCH"
  git clean -xfd
  git submodule update --init --recursive
else
  rm -rf -- ..?* .[!.]* *
  git clone /website.git -b "$BRANCH" ./
  git submodule update --init --recursive
  git config advice.detachedHead false
  git checkout -b checkout
fi
