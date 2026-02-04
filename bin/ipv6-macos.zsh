#!/usr/bin/env zsh

set -e

ifconfig en0 | grep inet6 | trim-whitespace
