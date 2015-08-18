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

TEST(Run_WasmModule_Return114) {
  static const byte kReturnValue = 114;
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f(&zone);
  f.ReturnType(kAstInt32);
  f.Exported(1);
  byte code[] = {WASM_RETURN(WASM_INT8(kReturnValue))};
  f.AddBody(code, sizeof(code));
  builder.AddFunction(f.Build());
  WasmModuleIndex module = builder.WriteAndBuild(&zone);
  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, module.Begin(), module.End());
  CHECK_EQ(kReturnValue, result);
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
  WasmModuleIndex module = builder.WriteAndBuild(&zone);
  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, module.Begin(), module.End());
  CHECK_EQ(99, result);
}

TEST(Run_WasmModule_ReadLoadedDataSegment) {
  static const byte kDataSegmentDest0 = 12;
  Zone zone;
  WasmModuleBuilder builder(&zone);
  WasmFunctionBuilder f(&zone);
  f.ReturnType(kAstInt32);
  f.Exported(1);
  byte code[] = {WASM_RETURN(WASM_LOAD_MEM(kMemInt32,WASM_INT8(kDataSegmentDest0)))};
  f.AddBody(code, sizeof(code));
  builder.AddFunction(f.Build());
  byte data[] = {0xaa, 0xbb, 0xcc, 0xdd};
  builder.AddDataSegment(
      WasmDataSegmentEncoder(&zone, data, sizeof(data), kDataSegmentDest0));
  WasmModuleIndex module = builder.WriteAndBuild(&zone);
  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, module.Begin(), module.End());
  CHECK_EQ(0xddccbbaa, result);
}
