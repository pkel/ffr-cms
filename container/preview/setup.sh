#!/usr/bin/env bash
set -xe

# create user and working directory
mkdir -p /home/ffr
useradd ffr -d /home/ffr
chown -R ffr:ffr /home/ffr

# install hugo
cd /install
tar -xf hugo.tar.gz
cp hugo /usr/bin
cd /
rm -r /install
