#!/bin/sh

if ! cargo r --example=test_parser "$1" &>/dev/null ; then
  echo "$1"
fi
