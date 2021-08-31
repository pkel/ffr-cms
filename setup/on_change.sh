#!/bin/bash

# This script monitors the website git repository for local changes.
# Three things happen on each change:
# 1. Pull remote website repo, auto-merge preferring remote changes
# 2. Push local changes
# 3. Compile website

set -e

branch=master
gitdir=${GITDIR:-$(pwd)/_db}
outdir=${OUTDIR:-$(pwd)/_www}

if [ "$1" == "watch" ] ; then
  echo "$gitdir/.git/refs/heads/$branch" | entr -n bash "$0"
  exit 0
else
  git -C "$gitdir" reset --hard
  git -C "$gitdir" switch "$branch"
  git -C "$gitdir" pull --rebase --strategy recursive --strategy-option ours \
    origin "$branch"
  git -C "$gitdir" push origin "$branch"
  git -C "$gitdir" submodule update --init --recursive
  (cd "$gitdir" && hugo --destination="$outdir" -D --cleanDestinationDir)
fi
