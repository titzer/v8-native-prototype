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
void TestModule(const WasmModuleIndex& module, int32_t expected_result) {
  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, module.Begin(), module.End());
  CHECK_EQ(expected_result, result);
}
}  // namespace


TEST(Run_WasmModule_Return114) {
  static const int32_t kReturnValue = 114;
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f(&zone);
  f.ReturnType(kAstInt32);
  f.Exported(1);
  byte code[] = {WASM_RETURN(WASM_INT8(kReturnValue))};
  f.AddBody(code, sizeof(code));
  builder.AddFunction(f.Build());
  TestModule(builder.BuildAndWrite(&zone), kReturnValue);
}


TEST(Run_WasmModule_CallAdd) {
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f1(&zone);
  f1.ReturnType(kAstInt32);
  f1.AddParam(kAstInt32);
  f1.AddParam(kAstInt32);
  byte code1[] = {
      WASM_RETURN(WASM_INT32_ADD(WASM_GET_LOCAL(0), WASM_GET_LOCAL(1)))};
  f1.AddBody(code1, sizeof(code1));
  builder.AddFunction(f1.Build());
  WasmFunctionBuilder f2(&zone);
  f2.ReturnType(kAstInt32);
  f2.Exported(1);
  byte code2[] = {
      WASM_RETURN(WASM_CALL_FUNCTION(0, WASM_INT8(77), WASM_INT8(22)))};
  f2.AddBody(code2, sizeof(code2));
  builder.AddFunction(f2.Build());
  TestModule(builder.BuildAndWrite(&zone), 99);
}


TEST(Run_WasmModule_ReadLoadedDataSegment) {
  static const byte kDataSegmentDest0 = 12;
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f(&zone);
  f.ReturnType(kAstInt32);
  f.Exported(1);
  byte code[] = {
      WASM_RETURN(WASM_LOAD_MEM(kMemInt32, WASM_INT8(kDataSegmentDest0)))};
  f.AddBody(code, sizeof(code));
  builder.AddFunction(f.Build());
  byte data[] = {0xaa, 0xbb, 0xcc, 0xdd};
  builder.AddDataSegment(
      WasmDataSegmentEncoder(&zone, data, sizeof(data), kDataSegmentDest0));
  TestModule(builder.BuildAndWrite(&zone), 0xddccbbaa);
}


TEST(Run_WasmModule_CheckMemoryIsZero) {
  static const int kCheckSize = 16 * 1024;
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f(&zone);
  f.ReturnType(kAstInt32);
  f.LocalInt32Count(1);
  f.Exported(1);
  byte code[] = {
      WASM_WHILE(
          WASM_INT32_SLT(WASM_GET_LOCAL(0), WASM_INT32(kCheckSize)),
          WASM_IF_THEN(WASM_LOAD_MEM(kMemInt32, WASM_GET_LOCAL(0)),
                       WASM_RETURN(WASM_INT8(-1)), WASM_INC_LOCAL_BY(0, 4))),
      WASM_INT8(11)};
  f.AddBody(code, sizeof(code));
  builder.AddFunction(f.Build());
  TestModule(builder.BuildAndWrite(&zone), 11);
}


TEST(Run_WasmModule_CallMain_recursive) {
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f(&zone);
  f.ReturnType(kAstInt32);
  f.LocalInt32Count(1);
  f.Exported(1);
  byte code[] = {
      WASM_BLOCK(
          2,                                                             // --
          WASM_SET_LOCAL(0, WASM_LOAD_MEM(kMemInt32, WASM_ZERO)),        // --
          WASM_IF_THEN(WASM_INT32_SLT(WASM_GET_LOCAL(0), WASM_INT8(5)),  // --
                       WASM_BLOCK(2,                                     // --
                                  WASM_STORE_MEM(kMemInt32, WASM_ZERO,   // --
                                                 WASM_INC_LOCAL(0)),     // --
                                  WASM_RETURN(WASM_CALL_FUNCTION0(0))),  // --
                       WASM_RETURN(WASM_INT8(55))))                      // --
  };
  f.AddBody(code, sizeof(code));
  builder.AddFunction(f.Build());
  TestModule(builder.BuildAndWrite(&zone), 55);
}
