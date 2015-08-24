#!/bin/bash

set -e

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

cd v8
gclient update
cd ..
