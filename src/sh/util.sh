#!/usr/bin/env bash

check_exe_exists() {
  if ! command -v "$1" &> /dev/null ; then
    echo "$1 required, please install $2"
    exit 901
  fi
}
