#!/bin/bash

set -e
set -x

# Move to a location relative to the script so it runs
# from anywhere.
cd $(dirname ${BASH_SOURCE[0]})/..

if [[ ! -d depot_tools ]]; then
  echo "Cloning depot_tools"
  git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
fi

export PATH=$PWD/depot_tools:$PATH

if [[ ! -d v8 ]]; then
  echo "Fetching v8"
  mkdir v8
  cd v8
  fetch v8
  ln -fs $PWD/.. v8/third_party/wasm
  cd ..
fi

if [[ ! -d v8/v8/test/mjsunit/wasm ]]; then
  ln -fs $PWD/test/mjsunit/wasm v8/v8/test/mjsunit/wasm
fi

cd v8
gclient update
cd ..
