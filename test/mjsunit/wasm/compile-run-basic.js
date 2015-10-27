// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

function bytes() {
  var buffer = new ArrayBuffer(arguments.length);
  var view = new Uint8Array(buffer);
  for (var i = 0; i < arguments.length; i++) {
    view[i] = arguments[i] | 0;
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

var kHeaderSize = 13;
var kAstStmt = 0;
var kAstI32 = 1;

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

var kReturnValue = 97;

var kBodySize = 2;
var kNameOffset = 15 + kBodySize + 1;

var data = bytes(
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
  kExprI8Const,               // --
  kReturnValue,               // --
  kDeclEnd,
  'm', 'a', 'i', 'n', 0       // name
);

assertEquals(kReturnValue, WASM.compileRun(data));
