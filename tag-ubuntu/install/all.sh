#!/bin/bash

set -e
cd $(dirname ${0})

./enable-apt-automatic-security-updates
./update-ubuntu
./install-spotify
./install-docker
./install-rcm

