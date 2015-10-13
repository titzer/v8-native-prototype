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
var kStmtNop = 0;
var kStmtBlock = 3;
var kExprI8Const = 0x10;
var kExprI32Sub = 0x41;
var kExprGetLocal = 0x15;
var kExprF64Lt = 0x99;
var kStmtReturn = 0x9;
var kExprCallFunction = 0x19;

function testCallFFI(ffi) {
  var kBodySize = 7;
  var kNameAddOffset = 28 + kBodySize + 1;
  var kNameMainOffset = kNameAddOffset + 4;

  var data = bytes(
    kDeclMemory,
    12, 12, 1,                  // memory
    // -- signatures
    kDeclSignatures, 1,
    2, kAstI32, kAstF64, kAstF64, // (f64,f64)->int
    // -- foreign function
    kDeclFunctions, 2,
    kDeclFunctionName | kDeclFunctionImport,
    0, 0,                       // signature index
    kNameAddOffset, 0, 0, 0,    // name offset
    // -- main function
    kDeclFunctionName | kDeclFunctionExport,
    0, 0,                       // signature index
    kNameMainOffset, 0, 0, 0,   // name offset
    kBodySize, 0,
    // main body
    kStmtReturn,                // --
    kExprCallFunction, 0,       // --
    kExprGetLocal, 0,           // --
    kExprGetLocal, 1,           // --
    // names
    kDeclEnd,
    'f', 'u', 'n', 0,           //  --
    'm', 'a', 'i', 'n', 0       //  --
  );

  print("instantiate FFI");
  var module = WASM.instantiateModule(data, ffi);
}

// everything is good.
(function() {
  var ffi = new Object();
  ffi.fun = function(a, b) { print(a, b); }
  testCallFFI(ffi);
})();


// FFI object should be an object.
assertThrows(function() {
  var ffi = 0;
  testCallFFI(ffi);
});


// FFI object should have a "fun" property.
assertThrows(function() {
  var ffi = new Object();
  testCallFFI(ffi);
});


// "fun" should be a JS function.
assertThrows(function() {
  var ffi = new Object();
  ffi.fun = new Object();
  testCallFFI(ffi);
});


// "fun" should be a JS function.
assertThrows(function() {
  var ffi = new Object();
  ffi.fun = 0;
  testCallFFI(ffi);
});
