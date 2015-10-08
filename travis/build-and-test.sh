#!/bin/bash

set -e
set -x

# Move to a location relative to the script so it runs
# from anywhere.
cd $(dirname ${BASH_SOURCE[0]})/..

cd v8/v8

echo "==== BUILDING V8 ===="
make x64.optdebug wasm=on werror=no

echo "==== unittests ===="
./out/x64.optdebug/unittests "--gtest_filter=Wasm*"

echo "==== cctest/test-run-wasm* ===="
./tools/run-tests.py \
  --no-presubmit --mode optdebug --arch x64 cctest/test-run-wasm*

echo "==== mjsunit/wasm/* ===="
./tools/run-tests.py \
  --no-presubmit --mode optdebug --arch x64 mjsunit/wasm/*
