// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// Flags: --expose-gc --stress-compaction

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

var kExprI32LoadMemL = 0x20;
var kExprI32Add = 0x40;
var kExprI32Sub = 0x41;

var kMemSize = 4096;

function genModule(memory) {
  var kBodySize = 27;
  var kNameMainOffset = 28 + kBodySize + 1;

  var data = bytes(
    kDeclMemory,
    12, 12, 1,                  // memory
    // -- signatures
    kDeclSignatures, 1,
    1, kAstI32, kAstI32,        // int->int
    // -- main function
    kDeclFunctions, 1,
    kDeclFunctionLocals | kDeclFunctionName | kDeclFunctionExport,
    0, 0,
    kNameMainOffset, 0, 0, 0,   // name offset
    1, 0,                       // local int32 count
    0, 0,                       // local int64 count
    0, 0,                       // local float32 count
    0, 0,                       // local float64 count
    kBodySize, 0,               // code size
    // main body: while(i) { if(mem[i]) return -1; i -= 4; } return 0;
    kExprBlock,2,
      kExprLoop,1,
        kExprIf,
          kExprGetLocal,0,
          kExprBr, 0,
            kExprIfThen,
              kExprI32LoadMemL,6,kExprGetLocal,0,
              kExprBr,2, kExprI8Const, 255,
              kExprSetLocal,0,
                kExprI32Sub,kExprGetLocal,0,kExprI8Const,4,
      kExprI8Const,0,
    // names
    kDeclEnd,
    'm', 'a', 'i', 'n', 0       //  --
  );

  return WASM.instantiateModule(data, null, memory);
}

function testPokeMemory() {
  var module = genModule(null);
  var buffer = module.memory;
  assertEquals(kMemSize, buffer.byteLength);

  var array = new Int8Array(buffer);
  assertEquals(kMemSize, array.length);

  for (var i = 0; i < kMemSize; i++) {
    assertEquals(0, array[i]);
  }

  for (var i = 0; i < 10; i++) {
    assertEquals(0, module.main(kMemSize - 4));

    array[kMemSize/2 + i] = 1;
    assertEquals(0, module.main(kMemSize/2 - 4));
    assertEquals(-1, module.main(kMemSize - 4));
    
    array[kMemSize/2 + i] = 0;
    assertEquals(0, module.main(kMemSize - 4));
  }
}

testPokeMemory();

function testSurvivalAcrossGc() {
  var checker = genModule(null).main;
  for (var i = 0; i < 5; i++) {
    print("gc run ", i);
    assertEquals(0, checker(kMemSize - 4));
    gc();
  }
}

testSurvivalAcrossGc();
testSurvivalAcrossGc();
testSurvivalAcrossGc();
testSurvivalAcrossGc();


function testPokeOuterMemory() {
  var buffer = new ArrayBuffer(kMemSize);
  var module = genModule(buffer);
  assertEquals(kMemSize, buffer.byteLength);

  var array = new Int8Array(buffer);
  assertEquals(kMemSize, array.length);

  for (var i = 0; i < kMemSize; i++) {
    assertEquals(0, array[i]);
  }

  for (var i = 0; i < 10; i++) {
    assertEquals(0, module.main(kMemSize - 4));

    array[kMemSize/2 + i] = 1;
    assertEquals(0, module.main(kMemSize/2 - 4));
    assertEquals(-1, module.main(kMemSize - 4));

    array[kMemSize/2 + i] = 0;
    assertEquals(0, module.main(kMemSize - 4));
  }
}

testPokeOuterMemory();

function testOuterMemorySurvivalAcrossGc() {
  var buffer = new ArrayBuffer(kMemSize);
  var checker = genModule(buffer).main;
  for (var i = 0; i < 5; i++) {
    print("gc run ", i);
    assertEquals(0, checker(kMemSize - 4));
    gc();
  }
}

testOuterMemorySurvivalAcrossGc();
testOuterMemorySurvivalAcrossGc();
testOuterMemorySurvivalAcrossGc();
testOuterMemorySurvivalAcrossGc();
