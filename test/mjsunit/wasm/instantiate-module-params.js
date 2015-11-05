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

function runSelect2(module, which, a, b) {
  assertEquals(which == 0 ? a : b, module.select(a, b));
}

function testSelect2(type) {
  var kBodySize = 2;
  var kNameOffset = 21 + kBodySize + 1;

  for (var which = 0; which < 2; which++) {
    print("type = " + type + ", which = " + which);

    var data = bytes(
      // -- memory
      kDeclMemory,
      12, 12, 1,                  // memory
      // -- signatures
      kDeclSignatures, 1,
      2, type, type, type,        // signature: (t,t)->t
      // -- select
      kDeclFunctions, 1,
      kDeclFunctionName | kDeclFunctionExport,
      0, 0,
      kNameOffset, 0, 0, 0,       // name offset
      kBodySize, 0,               // body size
      kExprGetLocal, which,       // --
      kDeclEnd,
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
  var kBodySize = 2;
  var kNameOffset = 29 + kBodySize + 1;

  for (var which = 0; which < 10; which++) {
    print("type = " + type + ", which = " + which);

    var t = type;
    var data = bytes(
      kDeclMemory,
      12, 12, 1,                  // memory
      // signatures
      kDeclSignatures, 1,
      10, t,t,t,t,t,t,t,t,t,t,t,  // (tx10)->t
      // main function
      kDeclFunctions, 1,
      kDeclFunctionName | kDeclFunctionExport,
      0, 0,
      kNameOffset, 0, 0, 0,       // name offset
      kBodySize, 0,               // body size
      kExprGetLocal, which,       // --
      kDeclEnd,
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


