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
var kStmtNop = 0;
var kExprI8Const = 0x10;
var kStmtReturn = 0x9;
var kReturnValue = 97;

var kBodySize = 3;
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
  kStmtReturn,                // --
  kExprI8Const,               // --
  kReturnValue,               // --
  kDeclEnd,
  'm', 'a', 'i', 'n', 0       // name
);

assertEquals(kReturnValue, WASM.compileRun(data));
