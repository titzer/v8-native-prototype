// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

function bytes() {
  var buffer = new ArrayBuffer(arguments.length);
  var view = new Uint8Array(buffer);
  for (var i = 0; i < arguments.length; i++) {
    var val = arguments[i];
    if ((typeof val) == "string") val = val.charCodeAt(0);
    view[i] = val | 0;
  }
  return buffer;
}

var kAstStmt = 0;
var kAstInt32 = 1;
var kStmtNop = 0;
var kExprInt8Const = 0x10;
var kStmtReturn = 0x9;
var kReturnValue = 117;
var kCodeStartOffset = 32;
var kCodeEndOffset = 35;
var kNameOffset = kCodeEndOffset;

var data = bytes(
  0, 0,                       // memory
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

var module = WASM.instantiateModule(data);

// Check the module exists.
assertFalse(module === undefined);
assertFalse(module === null);
assertFalse(module === 0);
assertEquals("object", typeof module);

// Check the memory is an ArrayBuffer.
var mem = module.memory;
assertFalse(mem === undefined);
assertFalse(mem === null);
assertFalse(mem === 0);
assertEquals("object", typeof mem);
assertTrue(mem instanceof ArrayBuffer);
for (var i = 0; i < 4; i++) {
  module.memory = 0;  // should be ignored
  assertEquals(mem, module.memory);
}

// Check the properties of the main function.
assertFalse(module.main === undefined);
assertFalse(module.main === null);
assertFalse(module.main === 0);
assertEquals("function", typeof module.main);

assertEquals(kReturnValue, module.main());
