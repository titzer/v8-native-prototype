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
var kStmtNop = 0;
var kExprInt8Const = 0x10;
var kStmtReturn = 0x9;

try {
  var data = bytes(
      0,       kAstStmt,  // signature
      3,       0,         // local int32 count
      4,       0,         // local int64 count
      5,       0,         // local float32 count
      6,       0,         // local float64 count
      kStmtNop            // body
  );

  WASM.verifyFunction(data);
  print("ok");
} catch (e) {
  assertTrue(false);
}


var threw = false;
try {
  var data = bytes(
      0,       kAstStmt,  // signature
      2,       0,         // local int32 count
      3,       0,         // local int64 count
      4,       0,         // local float32 count
      5,       0,         // local float64 count
      kStmtReturn, kStmtReturn   // body
  );

  WASM.verifyFunction(data);
  print("not ok");
} catch (e) {
  print("ok: " + e);
  threw = true;
}

assertTrue(threw);
