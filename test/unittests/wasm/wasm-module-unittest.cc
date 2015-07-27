// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "test/unittests/test-utils.h"

#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"

namespace v8 {
namespace internal {
namespace wasm {

class ModuleVerifyTest : public TestWithZone {};

#define EXPECT_VERIFIES(data)                                    \
  do {                                                           \
    ModuleResult result =                                        \
        DecodeWasmModule(nullptr, data, data + arraysize(data)); \
    EXPECT_TRUE(result.ok());                                    \
  } while (false)


#define EXPECT_FAILURE(data)                                     \
  do {                                                           \
    ModuleResult result =                                        \
        DecodeWasmModule(nullptr, data, data + arraysize(data)); \
    EXPECT_FALSE(result.ok());                                   \
  } while (false)


static const LocalType kLocalTypes[] = {kAstInt32, kAstInt64, kAstFloat32,
                                        kAstFloat64};

TEST_F(ModuleVerifyTest, DecodeEmpty) {
  static const byte data[] = {0, 0, 0, 0, 0, 0, 0, 0};

  EXPECT_VERIFIES(data);
}


TEST_F(ModuleVerifyTest, DecodeOneEmptyVoidVoidFunction) {
  static const byte data[] = {
      1,           // function count
      0, 0,        // signature: void -> void
      0, 0, 0, 0,  // name offset
      0, 0, 0, 0,  // code start offset
      0, 0, 0, 0,  // code end offset
      1, 2,        // local int32 count
      3, 4,        // local int64 count
      5, 6,        // local float32 count
      7, 8,        // local float64 count
      0,           // exported
      1            // external
  };

  {
    // Should decode to exactly one function.
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(1, result.val->functions->size());
    WasmFunction* function = &result.val->functions->back();

    EXPECT_EQ(0, function->name_offset);
    EXPECT_EQ(0, function->code_start_offset);
    EXPECT_EQ(0, function->code_end_offset);

    EXPECT_EQ(513, function->local_int32_count);
    EXPECT_EQ(1027, function->local_int64_count);
    EXPECT_EQ(1541, function->local_float32_count);
    EXPECT_EQ(2055, function->local_float64_count);

    EXPECT_EQ(false, function->exported);
    EXPECT_EQ(true, function->external);
  }

  for (size_t size = 0; size < arraysize(data); size++) {
    // Should fall off end of module bytes.
    ModuleResult result = DecodeWasmModule(nullptr, data, data + size);
    EXPECT_FALSE(result.ok());
  }
}


TEST_F(ModuleVerifyTest, DecodeModule_OneFunctionWithNopBody) {
  static const byte data[] = {
      1,                 // function count
      0,       0,        // signature: void -> void
      0,       0, 0, 0,  // name offset
      24,      0, 0, 0,  // code start offset
      25,      0, 0, 0,  // code end offset
      1,       2,        // local int32 count
      3,       4,        // local int64 count
      5,       6,        // local float32 count
      7,       8,        // local float64 count
      0,                 // exported
      0,                 // external
      kStmtNop           // body
  };

  {
    // Should decode to exactly one function.
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(1, result.val->functions->size());
    WasmFunction* function = &result.val->functions->back();

    EXPECT_EQ(0, function->name_offset);
    EXPECT_EQ(24, function->code_start_offset);
    EXPECT_EQ(25, function->code_end_offset);

    EXPECT_EQ(false, function->exported);
    EXPECT_EQ(false, function->external);
  }
}


TEST_F(ModuleVerifyTest, DecodeSignature_v_v) {
  static const byte data[] = {0, 0};
  Zone zone;
  FunctionSig* sig =
      DecodeFunctionSignatureForTesting(&zone, data, data + arraysize(data));

  EXPECT_TRUE(sig != nullptr);
  EXPECT_EQ(0, sig->parameter_count());
  EXPECT_EQ(0, sig->return_count());
}


TEST_F(ModuleVerifyTest, DecodeSignature_t_v) {
  Zone zone;

  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType ret_type = kLocalTypes[i];
    const byte data[] = {0, static_cast<byte>(ret_type)};
    FunctionSig* sig =
        DecodeFunctionSignatureForTesting(&zone, data, data + arraysize(data));

    EXPECT_TRUE(sig != nullptr);
    EXPECT_EQ(0, sig->parameter_count());
    EXPECT_EQ(1, sig->return_count());
    EXPECT_EQ(ret_type, sig->GetReturn());
  }
}


TEST_F(ModuleVerifyTest, DecodeSignature_v_t) {
  Zone zone;

  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType param_type = kLocalTypes[i];
    const byte data[] = {1, 0, static_cast<byte>(param_type)};
    FunctionSig* sig =
        DecodeFunctionSignatureForTesting(&zone, data, data + arraysize(data));

    EXPECT_TRUE(sig != nullptr);
    EXPECT_EQ(1, sig->parameter_count());
    EXPECT_EQ(0, sig->return_count());
    EXPECT_EQ(param_type, sig->GetParam(0));
  }
}


TEST_F(ModuleVerifyTest, DecodeSignature_t_t) {
  Zone zone;

  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType ret_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kLocalTypes); j++) {
      LocalType param_type = kLocalTypes[j];
      const byte data[] = {1,                               // param count
                           static_cast<byte>(ret_type),     // ret
                           static_cast<byte>(param_type)};  // param
      FunctionSig* sig = DecodeFunctionSignatureForTesting(
          &zone, data, data + arraysize(data));

      EXPECT_TRUE(sig != nullptr);
      EXPECT_EQ(1, sig->parameter_count());
      EXPECT_EQ(1, sig->return_count());
      EXPECT_EQ(param_type, sig->GetParam(0));
      EXPECT_EQ(ret_type, sig->GetReturn());
    }
  }
}


TEST_F(ModuleVerifyTest, DecodeSignature_i_tt) {
  Zone zone;

  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType p0_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kLocalTypes); j++) {
      LocalType p1_type = kLocalTypes[j];
      const byte data[] = {2,                             // param count
                           static_cast<byte>(kAstInt32),  // ret
                           static_cast<byte>(p0_type),    // p0
                           static_cast<byte>(p1_type)};   // p1
      FunctionSig* sig = DecodeFunctionSignatureForTesting(
          &zone, data, data + arraysize(data));

      EXPECT_TRUE(sig != nullptr);
      EXPECT_EQ(2, sig->parameter_count());
      EXPECT_EQ(1, sig->return_count());
      EXPECT_EQ(p0_type, sig->GetParam(0));
      EXPECT_EQ(p1_type, sig->GetParam(1));
    }
  }
}


TEST_F(ModuleVerifyTest, DecodeSignature_off_end) {
  Zone zone;
  byte data[256];
  for (int p = 0; p <= 255; p = p + 1 + p * 3) {
    for (int i = 0; i <= p; i++) data[i] = static_cast<byte>(kAstInt32);
    data[0] = static_cast<byte>(p);

    for (int i = 0; i < p + 1; i++) {
      // Should fall off the end for all signatures.
      FunctionSig* sig =
          DecodeFunctionSignatureForTesting(&zone, data, data + i);
      EXPECT_EQ(nullptr, sig);
    }
  }
}


TEST_F(ModuleVerifyTest, DecodeSignature_invalid_type) {
  byte kInvalidType = 76;
  Zone zone;
  for (int i = 1; i < 3; i++) {
    byte data[] = {2, kAstInt32, kAstInt32, kAstInt32};
    data[i] = kInvalidType;
    FunctionSig* sig =
        DecodeFunctionSignatureForTesting(&zone, data, data + arraysize(data));
    EXPECT_EQ(nullptr, sig);
  }
}
}
}
}
