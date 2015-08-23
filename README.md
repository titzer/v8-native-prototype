[![Build Status](https://travis-ci.org/WebAssembly/v8-native-prototype.svg?branch=master)](https://travis-ci.org/WebAssembly/v8-native-prototype)

# WebAssembly V8 Native Prototype

This repository contains additional source code needed to build a prototype
implementation of WebAssembly incorporated into V8.

To build:

* Install depot_tools:
  http://dev.chromium.org/developers/how-tos/install-depot-tools
* Checkout this prototype:
```
git clone https://github.com/WebAssembly/v8-native-prototype.git
```
* Checkout V8:
```
mkdir v8
cd v8
fetch v8
cd v8
```
* Add symlink to prototype from v8:
```
ln -fs $PWD/../../v8-native-prototype third_party/wasm
```
* make x64.debug wasm=on
