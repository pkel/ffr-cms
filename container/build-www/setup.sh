#!/usr/bin/env bash
set -xe

# create user and working directory
mkdir -p /home/ffr
useradd ffr -d /home/ffr
chown -R ffr:ffr /home/ffr

# prepare target directory
mkdir /www
chown -R ffr:ffr /www
