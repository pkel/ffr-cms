#!/usr/bin/env bash
set -xe

# install dependencies and clean up afterwards
apt-get update
apt-get -y upgrade
apt-get install -y "$@"
rm -rf /var/lib/apt/lists/*
