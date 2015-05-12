#!/bin/bash

V8_DIR="$1"

if [[ ! -e "$V8_DIR/src/v8.h" ]]; then
 echo "Usage: patch <v8-dir>"
 exit 1
fi

PATCH=$PWD
pushd $V8_DIR

echo Patching from $PATCH into $V8_DIR...

cp -r $PATCH/src/* src/
cp -r $PATCH/test/* test/

patch -p1 < $PATCH/v8.gyp.diff
patch -p1 < $PATCH/cctest.gyp.diff
patch -p1 < $PATCH/unittests.gyp.diff

popd
