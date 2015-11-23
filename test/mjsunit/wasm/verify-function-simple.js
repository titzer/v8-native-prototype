// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

function bytes() {
  var buffer = new ArrayBuffer(arguments.length);
  var view = new Uint8Array(buffer);
  for (var i = 0; i < arguments.length; i++) {
    view[i] = arguments[i] | 0;
  }
  return buffer;
}

var kAstStmt = 0;
var kAstI32 = 1;

var kExprNop =    0x00;
var kExprBlock =  0x01;
var kExprLoop =   0x02;
var kExprIf =     0x03;
var kExprIfThen = 0x04;
var kExprSelect = 0x05;
var kExprBr = 0x06;
var kExprBrIf = 0x07;
var kExprI8Const = 0x09;
var kExprI32Const = 0x0a;
var kExprI64Const = 0x0b;
var kExprF64Const = 0x0c;
var kExprF32Const = 0x0d;
var kExprGetLocal = 0x0e;
var kExprSetLocal = 0x0f;
var kExprLoadGlobal = 0x10;
var kExprStoreGlobal = 0x11;
var kExprCallFunction = 0x12;
var kExprCallIndirect = 0x13;

try {
  var data = bytes(
      0,       kAstStmt,  // signature
      3,       0,         // local int32 count
      4,       0,         // local int64 count
      5,       0,         // local float32 count
      6,       0,         // local float64 count
      kExprNop            // body
  );

  WASM.verifyFunction(data);
  print("ok");
} catch (e) {
  assertTrue(false);
}


var threw = false;
try {
  var data = bytes(
      0,       kAstI32,   // signature
      2,       0,         // local int32 count
      3,       0,         // local int64 count
      4,       0,         // local float32 count
      5,       0,         // local float64 count
      kExprBlock, 2, kExprNop, kExprNop  // body
  );

  WASM.verifyFunction(data);
  print("not ok");
} catch (e) {
  print("ok: " + e);
  threw = true;
}

assertTrue(threw);
