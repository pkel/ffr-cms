#!/usr/bin/env bash
set -ex

exec hugo server --buildDrafts --bind 0.0.0.0 --port 3001 --liveReloadPort "$EXTERNAL_PORT"
