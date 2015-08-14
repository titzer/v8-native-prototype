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
var kExprInt32Sub = 0x41;
var kExprGetLocal = 0x15;
var kStmtReturn = 0x9;
var kCodeStartOffset = 32;
var kCodeEndOffset = 38;
var kNameOffset = kCodeEndOffset;

var data = bytes(
  0, 0,                       // globals
  1, 0,                       // functions
  0, 0,                       // data segments
  2, kAstInt32, kAstInt32, kAstInt32,               // signature: int, int -> int
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
  kExprInt32Sub,              // --
  kExprGetLocal,              // --
  0,                          // --
  kExprGetLocal,              // --
  1,                          // --
  's', 'u', 'b', 0            // name
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

// Check the properties of the sub function.
assertFalse(module.sub === undefined);
assertFalse(module.sub === null);
assertFalse(module.sub === 0);
assertEquals("function", typeof module.sub);

assertEquals(-55, module.sub(33, 88));
assertEquals(-55555, module.sub(33333, 88888));
assertEquals(-5555555, module.sub(3333333, 8888888));
