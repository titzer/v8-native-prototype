#!/bin/bash

set -e
set -x

# Move to a location relative to the script so it runs
# from anywhere.
cd $(dirname ${BASH_SOURCE[0]})/..

cd v8/v8

echo "==== BUILDING V8 ===="
if [[ "${TRAVIS:-}" == "true" ]]; then
  make ${V8_ARCH}.${V8_MODE} wasm=on werror=no
else
  # For local builds.
  make ${V8_ARCH}.${V8_MODE} -j32
fi

echo "==== unittests ===="
./out/${V8_ARCH}.${V8_MODE}/unittests "--gtest_filter=Wasm*"

echo "==== cctest/test-run-wasm* ===="
./tools/run-tests.py \
  --no-presubmit --mode ${V8_MODE} --arch ${V8_ARCH} cctest/test-run-wasm*

echo "==== mjsunit/wasm/* ===="
./tools/run-tests.py \
  --no-presubmit --mode ${V8_MODE} --arch ${V8_ARCH} mjsunit/wasm/*
