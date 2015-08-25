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
var kExprCallFunction = 0x19;

function testCallFFI(ffi) {
  var kModuleHeaderSize = 8;
  var kFunctionSize = 24;
  var kCodeStart = kModuleHeaderSize + (kFunctionSize + 2) + (kFunctionSize + 2);
  var kCodeEnd = kCodeStart + 7;
  var kNameAddOffset = kCodeEnd;
  var kNameMainOffset = kNameAddOffset + 4;

  var data = bytes(
    12, 1,                      // memory
    0, 0,                       // globals
    2, 0,                       // functions
    0, 0,                       // data segments
    // -- foreign function
    2, kAstInt32, kAstFloat64, kAstFloat64, // signature: (f64,f64)->int
    kNameAddOffset, 0, 0, 0,    // name offset
    0, 0, 0, 0,                 // code start offset
    0, 0, 0, 0,                 // code end offset
    0, 0,                       // local int32 count
    0, 0,                       // local int64 count
    0, 0,                       // local float32 count
    0, 0,                       // local float64 count
    0,                          // exported
    1,                          // external
    // -- main function
    2, kAstInt32, kAstFloat64, kAstFloat64, // signature: (f64,f64)->int
    kNameMainOffset, 0, 0, 0,   // name offset
    kCodeStart, 0, 0, 0,        // code start offset
    kCodeEnd, 0, 0, 0,          // code end offset
    0, 0,                       // local int32 count
    0, 0,                       // local int64 count
    0, 0,                       // local float32 count
    0, 0,                       // local float64 count
    1,                          // exported
    0,                          // external
    // main body
    kStmtReturn,                // --
    kExprCallFunction, 0,       // --
    kExprGetLocal, 0,           // --
    kExprGetLocal, 1,           // --
    // names
    'f', 'u', 'n', 0,           //  --
    'm', 'a', 'i', 'n', 0       //  --
  );

  print("instantiate FFI");
  var module = WASM.instantiateModule(data, ffi);
}

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


// everything is good.
(function() {
  var ffi = new Object();
  ffi.fun = function(a, b) { print(a, b); }
  testCallFFI(ffi);
})();
