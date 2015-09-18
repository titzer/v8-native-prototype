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
var kAstI32 = 1;
var kAstI64 = 2;
var kAstF32 = 3;
var kAstF64 = 4;
var kStmtNop = 0;
var kStmtBlock = 3;
var kExprI8Const = 0x10;
var kExprI32Sub = 0x41;
var kExprGetLocal = 0x15;
var kStmtReturn = 0x9;

function runSelect2(module, which, a, b) {
  assertEquals(which == 0 ? a : b, module.select(a, b));
}

function testSelect2(type) {
  var kCodeStartOffset = 34;
  var kCodeSize = 3;
  var kCodeEndOffset = kCodeStartOffset + kCodeSize;
  var kNameOffset = kCodeEndOffset;

  for (var which = 0; which < 2; which++) {
    print("type = " + type + ", which = " + which);

    var data = bytes(
      12, 1,                      // memory
      0, 0,                       // globals
      1, 0,                       // functions
      0, 0,                       // data segments
      2, type, type, type,        // signature: (t,t)->t
      kNameOffset, 0, 0, 0,       // name offset
      kCodeStartOffset, 0, 0, 0,  // code start offset
      kCodeEndOffset, 0, 0, 0,    // code end offset
      0, 0,                       // local int32 count
      0, 0,                       // local int64 count
      0, 0,                       // local float32 count
      0, 0,                       // local float64 count
      1,                          // exported
      0,                          // external
      kStmtReturn,                // --       
      kExprGetLocal, which,       // --
      's','e','l','e','c','t',0   // name
    );
    
    var module = WASM.instantiateModule(data);

    assertEquals("function", typeof module.select);
    runSelect2(module, which, 99, 97);
    runSelect2(module, which, -99, -97);

    if (type != kAstF32) {
      runSelect2(module, which, 0x80000000 | 0, 0x7fffffff | 0);
      runSelect2(module, which, 0x80000001 | 0, 0x7ffffffe | 0);
      runSelect2(module, which, 0xffffffff | 0, 0xfffffffe | 0);
      runSelect2(module, which, -2147483647, 2147483646);
      runSelect2(module, which, -2147483646, 2147483645);
      runSelect2(module, which, -2147483648, 2147483647);
    }

    if (type != kAstI32 && type != kAstI64) {
      runSelect2(module, which, -1.25, 5.25);
      runSelect2(module, which, Infinity, -Infinity);
    }
  }
}


testSelect2(kAstI32);
testSelect2(kAstF32);
testSelect2(kAstF64);


function runSelect10(module, which, a, b) {
  var x = -1;

  var result = [
    module.select(a, b, x, x, x, x, x, x, x, x),
    module.select(x, a, b, x, x, x, x, x, x, x),
    module.select(x, x, a, b, x, x, x, x, x, x),
    module.select(x, x, x, a, b, x, x, x, x, x),
    module.select(x, x, x, x, a, b, x, x, x, x),
    module.select(x, x, x, x, x, a, b, x, x, x),
    module.select(x, x, x, x, x, x, a, b, x, x),
    module.select(x, x, x, x, x, x, x, a, b, x),
    module.select(x, x, x, x, x, x, x, x, a, b),
    module.select(x, x, x, x, x, x, x, x, x, a)
  ];

  for (var i = 0; i < 10; i++) {
     if (which == i) assertEquals(a, result[i]);
     else if (which == i+1) assertEquals(b, result[i]);
     else assertEquals(x, result[i]);
  }
}

function testSelect10(type) {
  var kCodeStartOffset = 42;
  var kCodeSize = 3;
  var kCodeEndOffset = kCodeStartOffset + kCodeSize;
  var kNameOffset = kCodeEndOffset;

  for (var which = 0; which < 10; which++) {
    print("type = " + type + ", which = " + which);

    var t = type;
    var data = bytes(
      12, 1,                      // memory
      0, 0,                       // globals
      1, 0,                       // functions
      0, 0,                       // data segments
      10, t,t,t,t,t,t,t,t,t,t,t,  // signature: (tx10)->t
      kNameOffset, 0, 0, 0,       // name offset
      kCodeStartOffset, 0, 0, 0,  // code start offset
      kCodeEndOffset, 0, 0, 0,    // code end offset
      0, 0,                       // local int32 count
      0, 0,                       // local int64 count
      0, 0,                       // local float32 count
      0, 0,                       // local float64 count
      1,                          // exported
      0,                          // external
      kStmtReturn,                // --       
      kExprGetLocal, which,       // --
      's','e','l','e','c','t',0   // name
    );
    
    var module = WASM.instantiateModule(data);

    assertEquals("function", typeof module.select);
    runSelect10(module, which, 99, 97);
    runSelect10(module, which, -99, -97);

    if (type != kAstF32) {
      runSelect10(module, which, 0x80000000 | 0, 0x7fffffff | 0);
      runSelect10(module, which, 0x80000001 | 0, 0x7ffffffe | 0);
      runSelect10(module, which, 0xffffffff | 0, 0xfffffffe | 0);
      runSelect10(module, which, -2147483647, 2147483646);
      runSelect10(module, which, -2147483646, 2147483645);
      runSelect10(module, which, -2147483648, 2147483647);
    }

    if (type != kAstI32 && type != kAstI64) {
      runSelect10(module, which, -1.25, 5.25);
      runSelect10(module, which, Infinity, -Infinity);
    }
  }
}


testSelect10(kAstI32);
testSelect10(kAstF32);
testSelect10(kAstF64);


