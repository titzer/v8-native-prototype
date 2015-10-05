// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <stdlib.h>
#include <string.h>

#include "src/wasm/encoder.h"
#include "src/wasm/wasm-macro-gen.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"

#include "test/cctest/cctest.h"

using namespace v8::base;
using namespace v8::internal;
using namespace v8::internal::compiler;
using namespace v8::internal::wasm;


namespace {
void TestModule(WasmModuleIndex* module, int32_t expected_result) {
  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, module->Begin(), module->End());
  CHECK_EQ(expected_result, result);
}
}  // namespace


static const int kModuleHeaderSize = 8;
static const int kFunctionSize = 24;
#define MODULE_HEADER(globals_count, functions_count, data_segments_count) \
  16, 0, static_cast<uint8_t>(globals_count),                              \
      static_cast<uint8_t>(globals_count >> 8),                            \
      static_cast<uint8_t>(functions_count),                               \
      static_cast<uint8_t>(functions_count >> 8),                          \
      static_cast<uint8_t>(data_segments_count),                           \
      static_cast<uint8_t>(data_segments_count >> 8)

// A raw test that skips the WasmModuleBuilder.
TEST(Run_WasmModule_CallAdd_rev) {
  static const byte kCodeStartOffset0 =
      kModuleHeaderSize + 2 + kFunctionSize * 2;
  static const byte kCodeEndOffset0 = kCodeStartOffset0 + 6;
  static const byte kCodeStartOffset1 = kCodeEndOffset0;
  static const byte kCodeEndOffset1 = kCodeEndOffset0 + 7;
  static const byte data[] = {
      MODULE_HEADER(0, 2, 0),  // globals, functions, data segments
      // func#0 (main) ----------------------------------
      0, kAstI32,                  // signature: void -> int
      0, 0, 0, 0,                  // name offset
      kCodeStartOffset1, 0, 0, 0,  // code start offset
      kCodeEndOffset1, 0, 0, 0,    // code end offset
      0, 0,                        // local int32 count
      0, 0,                        // local int64 count
      0, 0,                        // local float32 count
      0, 0,                        // local float64 count
      1,                           // exported
      0,                           // external
      // func#1 -----------------------------------------
      2, kAstI32, kAstI32, kAstI32,  // signature: int,int -> int
      0, 0, 0, 0,                    // name offset
      kCodeStartOffset0, 0, 0, 0,    // code start offset
      kCodeEndOffset0, 0, 0, 0,      // code end offset
      0, 0,                          // local int32 count
      0, 0,                          // local int64 count
      0, 0,                          // local float32 count
      0, 0,                          // local float64 count
      0,                             // exported
      0,                             // external
      // body#0 -----------------------------------------
      kStmtReturn,       // --
      kExprI32Add,       // --
      kExprGetLocal, 0,  // --
      kExprGetLocal, 1,  // --
      // body#1 -----------------------------------------
      kStmtReturn,           // --
      kExprCallFunction, 1,  // --
      kExprI8Const, 77,      // --
      kExprI8Const, 22       // --
  };

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(99, result);
}


TEST(Run_WasmModule_Return114) {
  static const int32_t kReturnValue = 114;
  Zone zone;
  WasmModuleBuilder* builder = new(&zone) WasmModuleBuilder(&zone);
  uint8_t f_index = builder->AddFunction();
  WasmFunctionBuilder* f = builder->FunctionAt(f_index);
  f->ReturnType(kAstI32);
  f->Exported(1);
  byte code[] = {WASM_RETURN(WASM_I8(kReturnValue))};
  f->AddBody(code, sizeof(code));
  WasmModuleWriter* writer = builder->Build(&zone);
  TestModule(writer->WriteTo(&zone), kReturnValue);
}


TEST(Run_WasmModule_CallAdd) {
  Zone zone;
  WasmModuleBuilder* builder = new(&zone) WasmModuleBuilder(&zone);
  uint8_t f1_index = builder->AddFunction();
  WasmFunctionBuilder* f = builder->FunctionAt(f1_index);
  f->ReturnType(kAstI32);
  uint8_t param1 = f->AddParam(kAstI32);
  uint8_t param2 = f->AddParam(kAstI32);
  byte code1[] = {
      WASM_RETURN(
          WASM_I32_ADD(WASM_GET_LOCAL(param1), WASM_GET_LOCAL(param2)))};
  uint32_t local_indices1[] = {3, 5};
  f->AddBody(code1, sizeof(code1), local_indices1, sizeof(local_indices1)/4);
  uint8_t f2_index = builder->AddFunction();
  f = builder->FunctionAt(f2_index);
  f->ReturnType(kAstI32);
  f->Exported(1);
  byte code2[] = {
      WASM_RETURN(WASM_CALL_FUNCTION(f1_index, WASM_I8(77), WASM_I8(22)))};
  f->AddBody(code2, sizeof(code2));
  WasmModuleWriter* writer = builder->Build(&zone);
  TestModule(writer->WriteTo(&zone), 99);
}


TEST(Run_WasmModule_ReadLoadedDataSegment) {
  static const byte kDataSegmentDest0 = 12;
  Zone zone;
  WasmModuleBuilder* builder = new(&zone) WasmModuleBuilder(&zone);
  uint8_t f_index = builder->AddFunction();
  WasmFunctionBuilder* f = builder->FunctionAt(f_index);
  f->ReturnType(kAstI32);
  f->Exported(1);
  byte code[] = {
      WASM_RETURN(WASM_LOAD_MEM(kMemI32, WASM_I8(kDataSegmentDest0)))};
  f->AddBody(code, sizeof(code));
  byte data[] = {0xaa, 0xbb, 0xcc, 0xdd};
  builder->AddDataSegment(
      new(&zone) WasmDataSegmentEncoder(
          &zone, data, sizeof(data), kDataSegmentDest0));
  WasmModuleWriter* writer = builder->Build(&zone);
  TestModule(writer->WriteTo(&zone), 0xddccbbaa);
}


TEST(Run_WasmModule_CheckMemoryIsZero) {
  static const int kCheckSize = 16 * 1024;
  Zone zone;
  WasmModuleBuilder* builder = new(&zone) WasmModuleBuilder(&zone);
  uint8_t f_index = builder->AddFunction();
  WasmFunctionBuilder* f = builder->FunctionAt(f_index);
  f->ReturnType(kAstI32);
  uint8_t localIndex = f->AddLocal(kAstI32);
  f->Exported(1);
  byte code[] = {
      WASM_WHILE(
          WASM_I32_LTS(WASM_GET_LOCAL(localIndex), WASM_I32(kCheckSize)),
          WASM_IF_THEN(WASM_LOAD_MEM(kMemI32, WASM_GET_LOCAL(localIndex)),
                       WASM_RETURN(WASM_I8(-1)),
                       WASM_INC_LOCAL_BY(localIndex, 4))),
      WASM_I8(11)};
  uint32_t local_indices[] = {5, 15, 20, 23};
  f->AddBody(code, sizeof(code), local_indices, sizeof(local_indices)/4);
  WasmModuleWriter* writer = builder->Build(&zone);
  TestModule(writer->WriteTo(&zone), 11);
}


TEST(Run_WasmModule_CallMain_recursive) {
  Zone zone;
  WasmModuleBuilder* builder = new(&zone) WasmModuleBuilder(&zone);
  uint8_t f_index = builder->AddFunction();
  WasmFunctionBuilder* f = builder->FunctionAt(f_index);
  f->ReturnType(kAstI32);
  uint8_t localIndex = f->AddLocal(kAstI32);
  f->Exported(1);
  byte code[] = {
      WASM_BLOCK(
          2, 
          WASM_SET_LOCAL(localIndex, WASM_LOAD_MEM(kMemI32, WASM_ZERO)),
          WASM_IF_THEN(WASM_I32_LTS(WASM_GET_LOCAL(localIndex), WASM_I8(5)),
                       WASM_BLOCK(2,
                                  WASM_STORE_MEM(kMemI32, WASM_ZERO,
                                                 WASM_INC_LOCAL(localIndex)),
                                  WASM_RETURN(WASM_CALL_FUNCTION0(0))),
                       WASM_RETURN(WASM_I8(55))))
  };
  uint32_t local_indices[] = {3, 11, 21, 24};
  f->AddBody(code, sizeof(code), local_indices, sizeof(local_indices)/4);
  WasmModuleWriter* writer = builder->Build(&zone);
  TestModule(writer->WriteTo(&zone), 55);
}
