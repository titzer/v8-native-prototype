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
var kExprTableSwitch = 0x08;
var kExprReturn = 0x14;
var kExprUnreachable = 0x15;
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
  var kBodySize1 = 1;
  var kMainOffset = 6 + kFuncWithBody + kBodySize1 + 1;

  var ffi = new Object();
  ffi.add = (function(a, b) { return a + b | 0; });

  return WASM.instantiateModule(bytes(
    // -- signatures
    kDeclSignatures, 1,
    0, kAstStmt, // void -> void
    // -- function #0 (unreachable)
    kDeclFunctions, 1,
    kDeclFunctionName | kDeclFunctionExport,
    0, 0,                      // signature offset
    kMainOffset, 0, 0, 0,      // name offset
    kBodySize1, 0,             // body size
    kExprUnreachable,
    kDeclEnd,
    'm', 'a', 'i', 'n', 0      // name
  ), ffi);
})();

// Check the module exists.
assertFalse(module === undefined);
assertFalse(module === null);
assertFalse(module === 0);
assertEquals("object", typeof module);
assertEquals("function", typeof module.main);

var exception = "";
try {
    assertEquals(0, module.main());
} catch(e) {
    print("correctly caught: " + e);
    exception = e;
}
assertEquals("unreachable", exception);
