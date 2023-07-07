#!/usr/bin/env bash
set -xe

# create user and working directory
mkdir -p /home/ffr
useradd ffr -d /home/ffr
chown -R ffr:ffr /home/ffr

# install dependencies and clean up afterwards
apt-get update
apt-get -y upgrade
apt-get install -y \
  entr git
apt-get clean
rm -rf /var/lib/apt/lists/*

# prepare target directory
mkdir /checkout
chown -R ffr:ffr /checkout
