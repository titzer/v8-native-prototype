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

function testCallFFI(func, check) {
  var kModuleHeaderSize = 8;
  var kFunctionSize = 24;
  var kCodeStart = kModuleHeaderSize + (kFunctionSize + 2) + (kFunctionSize + 2);
  var kCodeEnd = kCodeStart + 7;
  var kNameAddOffset = kCodeEnd;
  var kNameMainOffset = kNameAddOffset + 4;

  var ffi = new Object();
  ffi.fun = func;

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

  var module = WASM.instantiateModule(data, ffi);

  assertEquals("function", typeof module.main);

  for (var i = 0; i < 100000; i += 10003) {
    var a = 22.5 + i, b = 10.5 + i;
    var r = module.main(a, b);
    check(r, a, b);
  }
}

var global = (function() { return this; })();
var params = [-99, -99, -99, -99];
var was_called = false;
var length = -1;

function FOREIGN_SUB(a, b) {
  print("FOREIGN_SUB(" + a + ", " + b + ")");
  was_called = true;
  params[0] = this;
  params[1] = a;
  params[2] = b;
  return (a - b) | 0;
}

function check_FOREIGN_SUB(r, a, b) {
    assertEquals(a - b | 0, r);
    assertTrue(was_called);
    assertEquals(global, params[0]);  // sloppy mode
    assertEquals(a, params[1]);
    assertEquals(b, params[2]);
    was_called = false;
}

testCallFFI(FOREIGN_SUB, check_FOREIGN_SUB);


function FOREIGN_ABCD(a, b, c, d) {
  print("FOREIGN_ABCD(" + a + ", " + b + ", " + c + ", " + d + ")");
  was_called = true;
  params[0] = this;
  params[1] = a;
  params[2] = b;
  params[3] = c;
  params[4] = d;
  return (a * b * 6) | 0;
}

function check_FOREIGN_ABCD(r, a, b) {
    assertEquals((a * b * 6) | 0, r);
    assertTrue(was_called);
    assertEquals(global, params[0]);  // sloppy mode.
    assertEquals(a, params[1]);
    assertEquals(b, params[2]);
    assertEquals(undefined, params[3]);
    assertEquals(undefined, params[4]);
    was_called = false;
}

testCallFFI(FOREIGN_ABCD, check_FOREIGN_ABCD);

function FOREIGN_ARGUMENTS0() {
  print("FOREIGN_ARGUMENTS0");
  was_called = true;
  length = arguments.length;
  for (var i = 0; i < arguments.length; i++) {
    params[i] = arguments[i];
  }
  return (arguments[0] * arguments[1] * 7) | 0;
}

function FOREIGN_ARGUMENTS1(a) {
  print("FOREIGN_ARGUMENTS1", a);
  was_called = true;
  length = arguments.length;
  for (var i = 0; i < arguments.length; i++) {
    params[i] = arguments[i];
  }
  return (arguments[0] * arguments[1] * 7) | 0;
}

function FOREIGN_ARGUMENTS2(a, b) {
  print("FOREIGN_ARGUMENTS2", a, b);
  was_called = true;
  length = arguments.length;
  for (var i = 0; i < arguments.length; i++) {
    params[i] = arguments[i];
  }
  return (a * b * 7) | 0;
}

function FOREIGN_ARGUMENTS3(a, b, c) {
  print("FOREIGN_ARGUMENTS3", a, b, c);
  was_called = true;
  length = arguments.length;
  for (var i = 0; i < arguments.length; i++) {
    params[i] = arguments[i];
  }
  return (a * b * 7) | 0;
}

function FOREIGN_ARGUMENTS4(a, b, c, d) {
  print("FOREIGN_ARGUMENTS4", a, b, c, d);
  was_called = true;
  length = arguments.length;
  for (var i = 0; i < arguments.length; i++) {
    params[i] = arguments[i];
  }
  return (a * b * 7) | 0;
}

function check_FOREIGN_ARGUMENTS(r, a, b) {
  assertEquals((a * b * 7) | 0, r);
  assertTrue(was_called);
  assertEquals(2, length);
  assertEquals(a, params[0]);
  assertEquals(b, params[1]);
  was_called = false;
}

testCallFFI(FOREIGN_ARGUMENTS0, check_FOREIGN_ARGUMENTS);
testCallFFI(FOREIGN_ARGUMENTS1, check_FOREIGN_ARGUMENTS);
testCallFFI(FOREIGN_ARGUMENTS2, check_FOREIGN_ARGUMENTS);
testCallFFI(FOREIGN_ARGUMENTS3, check_FOREIGN_ARGUMENTS);
testCallFFI(FOREIGN_ARGUMENTS4, check_FOREIGN_ARGUMENTS);
