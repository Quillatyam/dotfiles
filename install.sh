#!/usr/bin/env bash

set -e
set -o pipefail

rcup -d ~/repositories/Quillatyam/dotfiles/ -t fish -t vim -t git -t tmux
