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
var kStmtNop = 0;
var kExprI8Const = 0x10;
var kStmtReturn = 0x9;
var kReturnValue = 117;

var kBodySize = 3;
var kNameOffset = 19 + kBodySize + 1;

var data = bytes(
  // -- memory
  kDeclMemory,
  10, 10, 1,
  // -- signatures
  kDeclSignatures, 1,
  0, kAstI32,                 // signature: void -> int
  // -- main function
  kDeclFunctions, 1,
  kDeclFunctionName | kDeclFunctionExport,
  0, 0,                       // signature index
  kNameOffset, 0, 0, 0,       // name offset
  kBodySize, 0,               // body size
  // -- body
  kStmtReturn,                // --
  kExprI8Const,               // --
  kReturnValue,               // --
  kDeclEnd,
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

assertEquals(1024, module.memory.byteLength);

// Check the properties of the main function.
assertFalse(module.main === undefined);
assertFalse(module.main === null);
assertFalse(module.main === 0);
assertEquals("function", typeof module.main);

assertEquals(kReturnValue, module.main());
