#!/usr/bin/env bash

set -xe

cd /checkout
hugo server --buildDrafts --bind 0.0.0.0 --port 3001
