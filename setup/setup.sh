#!/bin/bash

set -xe

sudo systemctl stop ffr-opium || true
sudo systemctl stop ffr-sync-build || true

sudo adduser --system cms --ingroup www-data

dune build --release

sudo cp _build/default/src/server/main.exe /usr/local/bin/ffr-opium
sudo cp _build/default/src/set_user/main.exe /usr/local/bin/ffr-set-user
sudo cp setup/on_change.sh ~cms/on_change.sh
sudo cp setup/systemd/* /etc/systemd/system/
sudo rsync -a --delete ./static ~cms/

sudo systemctl daemon-reload
sudo systemctl start ffr-opium
sudo systemctl start ffr-sync-build
sudo systemctl enable ffr-opium
sudo systemctl enable ffr-sync-build
