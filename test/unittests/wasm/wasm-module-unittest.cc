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

#define MODULE_HEADER(globals_count, functions_count, data_segments_count) \
  static_cast<uint8_t>(globals_count),                                     \
      static_cast<uint8_t>(globals_count >> 8),                            \
      static_cast<uint8_t>(functions_count),                               \
      static_cast<uint8_t>(functions_count >> 8),                          \
      static_cast<uint8_t>(data_segments_count),                           \
      static_cast<uint8_t>(data_segments_count >> 8)

TEST_F(ModuleVerifyTest, OneGlobal) {
  const byte data[] = {
      MODULE_HEADER(1, 0, 0),  // globals, functions, data_segments
      0, 0, 0, 0,              // name offset
      kMemInt32,               // memory type
      0,                       // exported
  };

  {
    // Should decode to exactly one global.
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(1, result.val->globals->size());
    EXPECT_EQ(0, result.val->functions->size());
    EXPECT_EQ(0, result.val->data_segments->size());

    WasmGlobal* global = &result.val->globals->back();

    EXPECT_EQ(0, global->name_offset);
    EXPECT_EQ(kMemInt32, global->type);
    EXPECT_EQ(0, global->offset);
    EXPECT_EQ(false, global->exported);
  }

  for (size_t size = 0; size < arraysize(data); size++) {
    // Should fall off end of module bytes.
    ModuleResult result = DecodeWasmModule(nullptr, data, data + size);
    EXPECT_FALSE(result.ok());
  }
}


TEST_F(ModuleVerifyTest, GlobalWithInvalidNameOffset) {
  const byte data[] = {
      MODULE_HEADER(1, 0, 0),  // globals, functions, data_segments
      0, 3, 0, 0,              // name offset
      kMemInt32,               // memory type
      0,                       // exported
  };

  ModuleResult result = DecodeWasmModule(nullptr, data, data + arraysize(data));
  EXPECT_FALSE(result.ok());
}


TEST_F(ModuleVerifyTest, GlobalWithInvalidMemoryType) {
  const byte data[] = {
      MODULE_HEADER(1, 0, 0),  // globals, functions, data_segments
      0, 0, 0, 0,              // name offset
      33,                      // memory type
      0,                       // exported
  };


  ModuleResult result = DecodeWasmModule(nullptr, data, data + arraysize(data));
  EXPECT_FALSE(result.ok());
}


TEST_F(ModuleVerifyTest, TwoGlobals) {
  const byte data[] = {
      MODULE_HEADER(2, 0, 0),  // globals, functions, data_segments
      0, 0, 0, 0,              // #0: name offset
      kMemFloat32,             // memory type
      0,                       // exported
      0, 0, 0, 0,              // #1: name offset
      kMemFloat64,             // memory type
      1,                       // exported
  };

  {
    // Should decode to exactly two globals.
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(2, result.val->globals->size());
    EXPECT_EQ(0, result.val->functions->size());
    EXPECT_EQ(0, result.val->data_segments->size());

    WasmGlobal* g0 = &result.val->globals->at(0);
    WasmGlobal* g1 = &result.val->globals->at(1);

    EXPECT_EQ(0, g0->name_offset);
    EXPECT_EQ(kMemFloat32, g0->type);
    EXPECT_EQ(0, g0->offset);
    EXPECT_EQ(false, g0->exported);

    EXPECT_EQ(0, g1->name_offset);
    EXPECT_EQ(kMemFloat64, g1->type);
    EXPECT_EQ(0, g1->offset);
    EXPECT_EQ(true, g1->exported);
  }

  for (size_t size = 0; size < arraysize(data); size++) {
    // Should fall off end of module bytes.
    ModuleResult result = DecodeWasmModule(nullptr, data, data + size);
    EXPECT_FALSE(result.ok());
  }
}


TEST_F(ModuleVerifyTest, OneEmptyVoidVoidFunction) {
  static const byte data[] = {
      MODULE_HEADER(0, 1, 0),  // globals, functions, data segments
      0, 0,                    // signature: void -> void
      0, 0, 0, 0,              // name offset
      0, 0, 0, 0,              // code start offset
      0, 0, 0, 0,              // code end offset
      1, 2,                    // local int32 count
      3, 4,                    // local int64 count
      5, 6,                    // local float32 count
      7, 8,                    // local float64 count
      0,                       // exported
      1                        // external
  };

  {
    // Should decode to exactly one function.
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(0, result.val->globals->size());
    EXPECT_EQ(1, result.val->functions->size());
    EXPECT_EQ(0, result.val->data_segments->size());

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


TEST_F(ModuleVerifyTest, OneFunctionWithNopBody) {
  static const byte kCodeStartOffset = 30;
  static const byte kCodeEndOffset = 31;

  static const byte data[] = {
      MODULE_HEADER(0, 1, 0),     // globals, functions, data segments
      0, 0,                       // signature: void -> void
      0, 0, 0, 0,                 // name offset
      kCodeStartOffset, 0, 0, 0,  // code start offset
      kCodeEndOffset, 0, 0, 0,    // code end offset
      1, 2,                       // local int32 count
      3, 4,                       // local int64 count
      5, 6,                       // local float32 count
      7, 8,                       // local float64 count
      0,                          // exported
      0,                          // external
      kStmtNop                    // body
  };

  CHECK_EQ(kCodeEndOffset, arraysize(data));

  {
    // Should decode to exactly one function.
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(1, result.val->functions->size());
    WasmFunction* function = &result.val->functions->back();

    EXPECT_EQ(0, function->name_offset);
    EXPECT_EQ(kCodeStartOffset, function->code_start_offset);
    EXPECT_EQ(kCodeEndOffset, function->code_end_offset);

    EXPECT_EQ(false, function->exported);
    EXPECT_EQ(false, function->external);
  }
}


TEST_F(ModuleVerifyTest, OneGlobalOneFunctionWithNopBodyOneDataSegment) {
  static const byte kCodeStartOffset = 49;
  static const byte kCodeEndOffset = 50;

  static const byte data[] = {
      MODULE_HEADER(1, 1, 1),  // globals, functions, data segments
      // global#0 --------------------------------------------------
      0, 0, 0, 0,  // name offset
      kMemUint8,   // memory type
      0,           // exported
      // func#0 ----------------------------------------------------
      0, 0,                       // signature: void -> void
      0, 0, 0, 0,                 // name offset
      kCodeStartOffset, 0, 0, 0,  // code start offset
      kCodeEndOffset, 0, 0, 0,    // code end offset
      1, 2,                       // local int32 count
      3, 4,                       // local int64 count
      5, 6,                       // local float32 count
      7, 8,                       // local float64 count
      0,                          // exported
      0,                          // external
      // segment#0 -------------------------------------------------
      0xae, 0xb3, 0x08, 0,  // dest addr
      15, 0, 0, 0,          // source offset
      5, 0, 0, 0,           // source size
      1,                    // init
      // rest ------------------------------------------------------
      kStmtNop,  // func#0 body

  };

  {
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(1, result.val->globals->size());
    EXPECT_EQ(1, result.val->functions->size());
    EXPECT_EQ(1, result.val->data_segments->size());

    WasmGlobal* global = &result.val->globals->back();

    EXPECT_EQ(0, global->name_offset);
    EXPECT_EQ(kMemUint8, global->type);
    EXPECT_EQ(0, global->offset);
    EXPECT_EQ(false, global->exported);

    WasmFunction* function = &result.val->functions->back();

    EXPECT_EQ(0, function->name_offset);
    EXPECT_EQ(kCodeStartOffset, function->code_start_offset);
    EXPECT_EQ(kCodeEndOffset, function->code_end_offset);

    EXPECT_EQ(false, function->exported);
    EXPECT_EQ(false, function->external);

    WasmDataSegment* segment = &result.val->data_segments->back();

    EXPECT_EQ(0x8b3ae, segment->dest_addr);
    EXPECT_EQ(15, segment->source_offset);
    EXPECT_EQ(5, segment->source_size);
    EXPECT_EQ(true, segment->init);
  }
}


TEST_F(ModuleVerifyTest, OneDataSegment) {
  const byte data[] = {
      MODULE_HEADER(0, 0, 1),  // globals, functions, data_segments
      0xaa, 0xbb, 0x09, 0,     // dest addr
      11, 0, 0, 0,             // source offset
      3, 0, 0, 0,              // source size
      1,                       // init
  };

  {
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(0, result.val->globals->size());
    EXPECT_EQ(0, result.val->functions->size());
    EXPECT_EQ(1, result.val->data_segments->size());

    WasmDataSegment* segment = &result.val->data_segments->back();

    EXPECT_EQ(0x9bbaa, segment->dest_addr);
    EXPECT_EQ(11, segment->source_offset);
    EXPECT_EQ(3, segment->source_size);
    EXPECT_EQ(true, segment->init);
  }

  for (size_t size = 0; size < arraysize(data); size++) {
    // Should fall off end of module bytes.
    ModuleResult result = DecodeWasmModule(nullptr, data, data + size);
    EXPECT_FALSE(result.ok());
  }
}


TEST_F(ModuleVerifyTest, TwoDataSegments) {
  const byte data[] = {
      MODULE_HEADER(0, 0, 2),  // globals, functions, data_segments
      0xee, 0xff, 0x07, 0,     // dest addr
      9, 0, 0, 0,              // #0: source offset
      4, 0, 0, 0,              // source size
      0,                       // init
      0xcc, 0xdd, 0x06, 0,     // #1: dest addr
      6, 0, 0, 0,              // source offset
      10, 0, 0, 0,             // source size
      1,                       // init
  };

  {
    ModuleResult result =
        DecodeWasmModule(nullptr, data, data + arraysize(data));
    EXPECT_TRUE(result.ok());
    EXPECT_EQ(0, result.val->globals->size());
    EXPECT_EQ(0, result.val->functions->size());
    EXPECT_EQ(2, result.val->data_segments->size());

    WasmDataSegment* s0 = &result.val->data_segments->at(0);
    WasmDataSegment* s1 = &result.val->data_segments->at(1);

    EXPECT_EQ(0x7ffee, s0->dest_addr);
    EXPECT_EQ(9, s0->source_offset);
    EXPECT_EQ(4, s0->source_size);
    EXPECT_EQ(false, s0->init);

    EXPECT_EQ(0x6ddcc, s1->dest_addr);
    EXPECT_EQ(6, s1->source_offset);
    EXPECT_EQ(10, s1->source_size);
    EXPECT_EQ(true, s1->init);
  }

  for (size_t size = 0; size < arraysize(data); size++) {
    // Should fall off end of module bytes.
    ModuleResult result = DecodeWasmModule(nullptr, data, data + size);
    EXPECT_FALSE(result.ok());
  }
}


class SignatureDecodeTest : public TestWithZone {};


TEST_F(SignatureDecodeTest, Ok_v_v) {
  static const byte data[] = {0, 0};
  Zone zone;
  FunctionSig* sig =
      DecodeFunctionSignatureForTesting(&zone, data, data + arraysize(data));

  EXPECT_TRUE(sig != nullptr);
  EXPECT_EQ(0, sig->parameter_count());
  EXPECT_EQ(0, sig->return_count());
}


TEST_F(SignatureDecodeTest, Ok_t_v) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType ret_type = kLocalTypes[i];
    const byte data[] = {0, static_cast<byte>(ret_type)};
    FunctionSig* sig =
        DecodeFunctionSignatureForTesting(zone(), data, data + arraysize(data));

    EXPECT_TRUE(sig != nullptr);
    EXPECT_EQ(0, sig->parameter_count());
    EXPECT_EQ(1, sig->return_count());
    EXPECT_EQ(ret_type, sig->GetReturn());
  }
}


TEST_F(SignatureDecodeTest, Ok_v_t) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType param_type = kLocalTypes[i];
    const byte data[] = {1, 0, static_cast<byte>(param_type)};
    FunctionSig* sig =
        DecodeFunctionSignatureForTesting(zone(), data, data + arraysize(data));

    EXPECT_TRUE(sig != nullptr);
    EXPECT_EQ(1, sig->parameter_count());
    EXPECT_EQ(0, sig->return_count());
    EXPECT_EQ(param_type, sig->GetParam(0));
  }
}


TEST_F(SignatureDecodeTest, Ok_t_t) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType ret_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kLocalTypes); j++) {
      LocalType param_type = kLocalTypes[j];
      const byte data[] = {1,                               // param count
                           static_cast<byte>(ret_type),     // ret
                           static_cast<byte>(param_type)};  // param
      FunctionSig* sig = DecodeFunctionSignatureForTesting(
          zone(), data, data + arraysize(data));

      EXPECT_TRUE(sig != nullptr);
      EXPECT_EQ(1, sig->parameter_count());
      EXPECT_EQ(1, sig->return_count());
      EXPECT_EQ(param_type, sig->GetParam(0));
      EXPECT_EQ(ret_type, sig->GetReturn());
    }
  }
}


TEST_F(SignatureDecodeTest, Ok_i_tt) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType p0_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kLocalTypes); j++) {
      LocalType p1_type = kLocalTypes[j];
      const byte data[] = {2,                             // param count
                           static_cast<byte>(kAstInt32),  // ret
                           static_cast<byte>(p0_type),    // p0
                           static_cast<byte>(p1_type)};   // p1
      FunctionSig* sig = DecodeFunctionSignatureForTesting(
          zone(), data, data + arraysize(data));

      EXPECT_TRUE(sig != nullptr);
      EXPECT_EQ(2, sig->parameter_count());
      EXPECT_EQ(1, sig->return_count());
      EXPECT_EQ(p0_type, sig->GetParam(0));
      EXPECT_EQ(p1_type, sig->GetParam(1));
    }
  }
}


TEST_F(SignatureDecodeTest, Fail_off_end) {
  byte data[256];
  for (int p = 0; p <= 255; p = p + 1 + p * 3) {
    for (int i = 0; i <= p; i++) data[i] = static_cast<byte>(kAstInt32);
    data[0] = static_cast<byte>(p);

    for (int i = 0; i < p + 1; i++) {
      // Should fall off the end for all signatures.
      FunctionSig* sig =
          DecodeFunctionSignatureForTesting(zone(), data, data + i);
      EXPECT_EQ(nullptr, sig);
    }
  }
}


TEST_F(SignatureDecodeTest, Fail_invalid_type) {
  byte kInvalidType = 76;
  for (int i = 1; i < 3; i++) {
    byte data[] = {2, kAstInt32, kAstInt32, kAstInt32};
    data[i] = kInvalidType;
    FunctionSig* sig =
        DecodeFunctionSignatureForTesting(zone(), data, data + arraysize(data));
    EXPECT_EQ(nullptr, sig);
  }
}


class FunctionVerifyTest : public TestWithZone {};


TEST_F(FunctionVerifyTest, Ok_v_v_empty) {
  byte data[] = {
      0,       kAstStmt,  // signature
      3,       0,         // local int32 count
      4,       0,         // local int64 count
      5,       0,         // local float32 count
      6,       0,         // local float64 count
      kStmtNop            // body
  };

  FunctionResult result = DecodeWasmFunction(nullptr, zone(), nullptr, data,
                                             data + arraysize(data));
  EXPECT_TRUE(result.ok());

  if (result.val && result.ok()) {
    WasmFunction* function = result.val;
    EXPECT_EQ(0, function->sig->parameter_count());
    EXPECT_EQ(0, function->sig->return_count());
    EXPECT_EQ(0, function->name_offset);
    EXPECT_EQ(arraysize(data) - 1, function->code_start_offset);
    EXPECT_EQ(arraysize(data), function->code_end_offset);
    EXPECT_EQ(3, function->local_int32_count);
    EXPECT_EQ(4, function->local_int64_count);
    EXPECT_EQ(5, function->local_float32_count);
    EXPECT_EQ(6, function->local_float64_count);
    EXPECT_FALSE(function->external);
    EXPECT_FALSE(function->exported);
  }
}
}
}
}
