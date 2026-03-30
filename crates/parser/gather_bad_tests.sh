#!/bin/sh

bad_tests=()
for test in ~/dev/dtc/tests/*.dts ; do
  if ! cargo r --example=test_parser "$test" &>/dev/null ; then
    bad_tests+=("$test")
  fi
done
echo Bad tests: $bad_tests
