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
var kAstInt64 = 2;
var kAstFloat32 = 3;
var kAstFloat64 = 4;
var kStmtNop = 0;
var kStmtBlock = 3;
var kExprInt8Const = 0x10;
var kExprInt32Sub = 0x41;
var kExprGetLocal = 0x15;
var kExprFloat64Lt = 0x99;
var kStmtReturn = 0x9;
var kCodeStartOffset = 34;
var kCodeEndOffset = 40;
var kNameOffset = kCodeEndOffset;

var data = bytes(
  12, 1,                      // memory
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

assertEquals(4096, module.memory.byteLength);

// Check the properties of the sub function.
assertFalse(module.sub === undefined);
assertFalse(module.sub === null);
assertFalse(module.sub === 0);
assertEquals("function", typeof module.sub);

assertEquals(-55, module.sub(33, 88));
assertEquals(-55555, module.sub(33333, 88888));
assertEquals(-5555555, module.sub(3333333, 8888888));


var kCodeStartOffset2 = 32;
var kCodeEndOffset2 = 33;
var kNameOffset2 = kCodeEndOffset2;

var data = bytes(
  12, 1,                      // memory
  0, 0,                       // globals
  1, 0,                       // functions
  0, 0,                       // data segments
  0, kAstStmt,                // signature: void -> void
  kNameOffset2, 0, 0, 0,      // name offset
  kCodeStartOffset2, 0, 0, 0, // code start offset
  kCodeEndOffset2, 0, 0, 0,   // code end offset
  0, 0,                       // local int32 count
  0, 0,                       // local int64 count
  0, 0,                       // local float32 count
  0, 0,                       // local float64 count
  1,                          // exported
  0,                          // external
  kStmtNop,                   // body
  'n', 'o', 'p', 0            // name
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

assertEquals(4096, module.memory.byteLength);

// Check the properties of the sub function.
assertFalse(module.nop === undefined);
assertFalse(module.nop === null);
assertFalse(module.nop === 0);
assertEquals("function", typeof module.nop);

assertEquals(undefined, module.nop());

(function testLt() {
  var kCodeStartOffset3 = 34;
  var kCodeSize3 = 8;
  var kCodeEndOffset3 = kCodeStartOffset3 + kCodeSize3;
  var kNameOffset3 = kCodeEndOffset3;

  var data = bytes(
    12, 1,                      // memory
    0, 0,                       // globals
    1, 0,                       // functions
    0, 0,                       // data segments
    2, kAstInt32, kAstFloat64, kAstFloat64, // signature: (f64,f64)->int
    kNameOffset3, 0, 0, 0,      // name offset
    kCodeStartOffset3, 0, 0, 0, // code start offset
    kCodeEndOffset3, 0, 0, 0,   // code end offset
    0, 0,                       // local int32 count
    0, 0,                       // local int64 count
    0, 0,                       // local float32 count
    0, 0,                       // local float64 count
    1,                          // exported
    0,                          // external
    kStmtBlock, 1,              // body
    kStmtReturn,                // --       
    kExprFloat64Lt,             // --
    kExprGetLocal, 0,           // --
    kExprGetLocal, 1,           // --
    'f', 'l', 't', 0            // name
  );

  var module = WASM.instantiateModule(data);

  assertEquals("function", typeof module.flt);
  assertEquals(1, module.flt(-2, -1));
  assertEquals(0, module.flt(7.3, 7.1));
  assertEquals(1, module.flt(7.1, 7.3));
})();
