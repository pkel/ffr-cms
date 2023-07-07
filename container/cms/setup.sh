#!/usr/bin/env bash
set -xe

# create user and working directory
mkdir /home/ffr
useradd ffr -d /home/ffr
chown -R ffr:ffr /home/ffr

# install dependencies and clean up afterwards
apt-get update
apt-get -y upgrade
apt-get install -y \
  imagemagick git libargon2-dev libev-dev libffi-dev libgmp-dev
apt-get clean
rm -rf /var/lib/apt/lists/*

# init website git repository
git init --bare --initial-branch master /website.git
chown -R ffr:ffr /website.git
