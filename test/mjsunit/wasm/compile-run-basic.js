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
var kAstInt32 = 1;
var kStmtNop = 0;
var kExprInt8Const = 0x10;
var kStmtReturn = 0x9;
var kReturnValue = 97;
var kCodeStartOffset = 30;
var kCodeEndOffset = 33;
var kNameOffset = kCodeEndOffset;

var data = bytes(
  0, 0,                       // globals
  1, 0,                       // functions
  0, 0,                       // data segments
  0, kAstInt32,               // signature: void -> int
  kNameOffset, 0, 0, 0,       // name offset
  kCodeStartOffset, 0, 0, 0,  // code start offset
  kCodeEndOffset, 0, 0, 0,    // code end offset
  0, 0,                       // local int32 count
  0, 0,                       // local int64 count
  0, 0,                       // local float32 count
  0, 0,                       // local float64 count
  1,                          // exported
  0,                          // external
  kStmtReturn,                // body
  kExprInt8Const,             // --
  kReturnValue,               // --
  'm', 'a', 'i', 'n', 0       // name
);

assertEquals(kReturnValue, WASM.compileRun(data));
