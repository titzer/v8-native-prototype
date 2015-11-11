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
  var kFuncWithBody = 9;
  var kFuncImported = 7;
  var kBodySize1 = 5;
  var kBodySize2 = 8;
  var kFuncTableSize = 8;
  var kSubOffset = 13 + kFuncWithBody + kBodySize1 + kFuncImported + kFuncWithBody + kBodySize2 + kFuncTableSize + 1;
  var kAddOffset = kSubOffset + 4;
  var kMainOffset = kAddOffset + 4;

  var ffi = new Object();
  ffi.add = (function(a, b) { return a + b | 0; });

  return WASM.instantiateModule(bytes(
    // -- signatures
    kDeclSignatures, 2,
    2, kAstI32, kAstI32, kAstI32, // int, int -> int
    3, kAstI32, kAstI32, kAstI32, kAstI32, // int, int, int -> int
    // -- function #0 (sub)
    kDeclFunctions, 3,
    kDeclFunctionName,
    0, 0,                         // signature offset
    kSubOffset, 0, 0, 0,          // name offset
    kBodySize1, 0,                // body size
    kExprI32Sub,                  // --
    kExprGetLocal, 0,             // --
    kExprGetLocal, 1,             // --
    // -- function #1 (add)
    kDeclFunctionName | kDeclFunctionImport,
    0, 0,                         // signature offset
    kAddOffset, 0, 0, 0,          // name offset
    // -- function #2 (main)
    kDeclFunctionName | kDeclFunctionExport,
    1, 0,                         // signature offset
    kMainOffset, 0, 0, 0,         // name offset
    kBodySize2, 0,                // body size
    kExprCallIndirect, 0,
    kExprGetLocal, 0,
    kExprGetLocal, 1,
    kExprGetLocal, 2,
    // -- function table
    kDeclFunctionTable,
    3,
    0, 0,
    1, 0,
    2, 0,
    kDeclEnd,
    's', 'u', 'b', 0,              // name
    'a', 'd', 'd', 0,              // name
    'm', 'a', 'i', 'n', 0          // name
  ), ffi);
})();

// Check the module exists.
assertFalse(module === undefined);
assertFalse(module === null);
assertFalse(module === 0);
assertEquals("object", typeof module);
assertEquals("function", typeof module.main);

assertEquals(5, module.main(0, 12, 7));
assertEquals(19, module.main(1, 12, 7));

var exception = "";
try {
    assertEquals(999, module.main(2, 12, 33));
} catch(e) {
    print("correctly caught: " + e);
    exception = e;
}
assertEquals("function signature mismatch", exception);

var exception = "";
try {
    assertEquals(999, module.main(3, 12, 33));
} catch(e) {
    print("correctly caught: " + e);
    exception = e;
}
assertEquals("function pointer out of bounds", exception);
