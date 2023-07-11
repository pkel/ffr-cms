#!/usr/bin/env bash
set -Eeuxo pipefail

echo "/website.git/refs/heads/$BRANCH" | entr -n bash "$1"
