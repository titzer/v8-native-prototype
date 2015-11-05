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

var kDeclMemory = 0x00;
var kDeclSignatures = 0x01;
var kDeclFunctions = 0x02;
var kDeclGlobals = 0x03;
var kDeclDataSegments = 0x04;
var kDeclFunctionTable = 0x05;
var kDeclEnd = 0x06;

var kDeclFunctionName   = 0x01;
var kDeclFunctionImport = 0x02;
var kDeclFunctionLocals = 0x04;
var kDeclFunctionExport = 0x08;

var kAstStmt = 0;
var kAstI32 = 1;
var kAstI64 = 2;
var kAstF32 = 3;
var kAstF64 = 4;

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

var kExprI32Sub = 0x41;
var kExprF64Lt = 0x99;

var module = (function () {
  var kBodySize = 5;
  var kNameOffset = 21 + kBodySize + 1;

  return WASM.instantiateModule(bytes(
    // -- memory
    kDeclMemory,
    12, 12, 1,
    // -- signatures
    kDeclSignatures, 1,
    2, kAstI32, kAstI32, kAstI32, // int, int -> int
    // -- functions
    kDeclFunctions, 1,
    kDeclFunctionName | kDeclFunctionExport,
    0, 0,
    kNameOffset, 0, 0, 0,         // name offset
    kBodySize, 0,
    // -- body
    kExprI32Sub,                  // --
    kExprGetLocal, 0,             // --
    kExprGetLocal, 1,             // --
    kDeclEnd,
    's', 'u', 'b', 0              // name
  ));
})();

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
assertEquals("function", typeof module.sub);

assertEquals(-55, module.sub(33, 88));
assertEquals(-55555, module.sub(33333, 88888));
assertEquals(-5555555, module.sub(3333333, 8888888));


var module = (function() {
  var kBodySize = 1;
  var kNameOffset2 = 19 + kBodySize + 1;

  return WASM.instantiateModule(bytes(
    // -- memory
    kDeclMemory,
    12, 12, 1,
    // -- signatures
    kDeclSignatures, 1,
    0, kAstStmt,                // signature: void -> void
    // -- functions
    kDeclFunctions, 1,
    kDeclFunctionName | kDeclFunctionExport,
    0, 0,                       // signature index
    kNameOffset2, 0, 0, 0,      // name offset
    kBodySize, 0,
    kExprNop,                   // body
    kDeclEnd,
    'n', 'o', 'p', 0            // name
  ));
})();

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
  var kBodySize = 5;
  var kNameOffset = 21 + kBodySize + 1;

  var data = bytes(
    // -- memory
    kDeclMemory,
    12, 12, 1,
    // -- signatures
    kDeclSignatures, 1,
    2, kAstI32, kAstF64, kAstF64, // (f64,f64)->int
    // -- functions
    kDeclFunctions, 1,
    kDeclFunctionName | kDeclFunctionExport,
    0, 0,                         // signature index
    kNameOffset, 0, 0, 0,         // name offset
    kBodySize, 0,
    // -- body
    kExprF64Lt,                   // --
    kExprGetLocal, 0,             // --
    kExprGetLocal, 1,             // --
    kDeclEnd,
    'f', 'l', 't', 0              // name
  );

  var module = WASM.instantiateModule(data);

  assertEquals("function", typeof module.flt);
  assertEquals(1, module.flt(-2, -1));
  assertEquals(0, module.flt(7.3, 7.1));
  assertEquals(1, module.flt(7.1, 7.3));
})();
