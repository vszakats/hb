#!/bin/sh -eu

cd "$(dirname "$0")"

jekyll build --incremental --watch "$@"
