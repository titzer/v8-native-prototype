// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "test/unittests/test-utils.h"

#include "src/v8.h"

#include "test/cctest/wasm/test-signatures.h"

#include "src/wasm/decoder.h"
#include "src/wasm/wasm-macro-gen.h"
#include "src/wasm/wasm-module.h"

namespace v8 {
namespace internal {
namespace wasm {

static const byte kCodeGetLocal0[] = {kExprGetLocal, 0};
static const byte kCodeGetLocal1[] = {kExprGetLocal, 1};
static const byte kCodeSetLocal0[] = {kExprSetLocal, 0, kExprI8Const, 0};

static const LocalType kLocalTypes[] = {kAstI32, kAstI64, kAstF32,
                                        kAstF64};
static const MemType kMemTypes[] = {
    kMemI8,   kMemU8, kMemI16,  kMemU16,  kMemI32,
    kMemU32, kMemI64, kMemU64, kMemF32, kMemF64};

static const WasmOpcode kInt32BinopOpcodes[] = {
    kExprI32Add,  kExprI32Sub,  kExprI32Mul,  kExprI32DivS,
    kExprI32DivU, kExprI32RemS, kExprI32RemU, kExprI32And,
    kExprI32Ior,  kExprI32Xor,  kExprI32Shl,  kExprI32ShrU,
    kExprI32ShrS,  kExprI32Eq,   kExprI32LtS,  kExprI32LeS,
    kExprI32LtU,  kExprI32LeU};


#define EXPECT_VERIFIES(env, x) Verify(kSuccess, env, x, x + arraysize(x))

#define EXPECT_FAILURE(env, x) Verify(kError, env, x, x + arraysize(x))

#define EXPECT_VERIFIES_INLINE(env, ...)                 \
  do {                                                   \
    static byte code[] = {__VA_ARGS__};                  \
    Verify(kSuccess, env, code, code + arraysize(code)); \
  } while (false)


#define EXPECT_FAILURE_INLINE(env, ...)                \
  do {                                                 \
    static byte code[] = {__VA_ARGS__};                \
    Verify(kError, env, code, code + arraysize(code)); \
  } while (false)

#define VERIFY(...)                                        \
  do {                                                     \
    static const byte code[] = {__VA_ARGS__};              \
    Verify(kSuccess, &env_v_i, code, code + sizeof(code)); \
  } while (false)


class WasmDecoderTest : public TestWithZone {
 public:
  WasmDecoderTest() : TestWithZone(), sigs() {
    init_env(&env_i_i, sigs.i_i());
    init_env(&env_v_v, sigs.v_v());
    init_env(&env_v_i, sigs.v_i());
    init_env(&env_i_f, sigs.i_f());
    init_env(&env_i_d, sigs.i_d());
    init_env(&env_l_l, sigs.l_l());
    init_env(&env_f_ff, sigs.f_ff());
    init_env(&env_d_dd, sigs.d_dd());
  }

  TestSignatures sigs;

  FunctionEnv env_i_i;
  FunctionEnv env_v_v;
  FunctionEnv env_v_i;
  FunctionEnv env_i_f;
  FunctionEnv env_i_d;
  FunctionEnv env_l_l;
  FunctionEnv env_f_ff;
  FunctionEnv env_d_dd;

  void init_env(FunctionEnv* env, FunctionSig* sig) {
    env->module = nullptr;
    env->sig = sig;
    env->local_int32_count = 0;
    env->local_int64_count = 0;
    env->local_float32_count = 0;
    env->local_float64_count = 0;
    env->SumLocals();
  }

  // A wrapper around VerifyWasmCode() that renders a nice failure message.
  void Verify(ErrorCode expected, FunctionEnv* env, const byte* start,
              const byte* end) {
    TreeResult result = VerifyWasmCode(env, start, end);
    if (result.error_code != expected) {
      ptrdiff_t pc = result.error_pc - result.start;
      ptrdiff_t pt = result.error_pt - result.start;
      std::ostringstream str;
      if (expected == kSuccess) {
        str << "Verification failed: " << result.error_code << " pc = +" << pc;
        if (result.error_pt) str << ", pt = +" << pt;
        str << ", msg = " << result.error_msg.get();
      } else {
        str << "Verification expected: " << expected << ", but got "
            << result.error_code;
        if (result.error_code != kSuccess) {
          str << " pc = +" << pc;
          if (result.error_pt) str << ", pt = +" << pt;
        }
      }
      FATAL(str.str().c_str());
    }
  }

  void TestBinop(WasmOpcode opcode, FunctionSig* success) {
    // Return(op(local[0], local[1]))
    byte code[] = {kStmtReturn, static_cast<byte>(opcode), kExprGetLocal, 0,
                   kExprGetLocal, 1};
    FunctionEnv env;
    init_env(&env, success);
    EXPECT_VERIFIES(&env, code);

    // Try all combinations of return and parameter types.
    for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
      for (size_t j = 0; j < arraysize(kLocalTypes); j++) {
        for (size_t k = 0; k < arraysize(kLocalTypes); k++) {
          LocalType types[] = {kLocalTypes[i], kLocalTypes[j], kLocalTypes[k]};
          if (types[0] != success->GetReturn(0) ||
              types[1] != success->GetParam(0) ||
              types[2] != success->GetParam(1)) {
            // Test signature mismatch.
            FunctionSig sig(1, 2, types);
            init_env(&env, &sig);
            EXPECT_FAILURE(&env, code);
          }
        }
      }
    }
  }

  void TestUnop(WasmOpcode opcode, FunctionSig* success) {
    TestUnop(opcode, success->GetReturn(), success->GetParam(0));
  }

  void TestUnop(WasmOpcode opcode, LocalType ret_type, LocalType param_type) {
    // Return(op(local[0]))
    byte code[] = {kStmtReturn, static_cast<byte>(opcode), kExprGetLocal, 0};
    FunctionEnv env;
    {
      LocalType types[] = {ret_type, param_type};
      FunctionSig sig(1, 1, types);
      init_env(&env, &sig);
      EXPECT_VERIFIES(&env, code);
    }

    // Try all combinations of return and parameter types.
    for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
      for (size_t j = 0; j < arraysize(kLocalTypes); j++) {
        LocalType types[] = {kLocalTypes[i], kLocalTypes[j]};
        if (types[0] != ret_type || types[1] != param_type) {
          // Test signature mismatch.
          FunctionSig sig(1, 1, types);
          init_env(&env, &sig);
          EXPECT_FAILURE(&env, code);
        }
      }
    }
  }
};


static FunctionEnv CreateInt32FunctionEnv(FunctionSig* sig, int count) {
  FunctionEnv env;
  env.module = nullptr;
  env.sig = sig;
  env.local_int32_count = count;
  env.local_float64_count = 0;
  env.local_float32_count = 0;
  env.total_locals = static_cast<unsigned>(count + sig->parameter_count());
  return env;
}


TEST_F(WasmDecoderTest, Int8Const) {
  byte code[] = {kExprI8Const, 0};
  for (int i = -128; i < 128; i++) {
    code[1] = static_cast<byte>(i);
    EXPECT_VERIFIES(&env_i_i, code);
  }
}


TEST_F(WasmDecoderTest, Int8Const_fallthru) {
  byte code[] = {kExprI8Const, 0, kExprI8Const, 1};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, Int32Const) {
  byte code[] = {kExprI32Const, 0, 0, 0, 0};
  int32_t* ptr = reinterpret_cast<int32_t*>(code + 1);
  const int kInc = 4498211;
  for (int32_t i = kMinInt; i < kMaxInt - kInc; i = i + kInc) {
    *ptr = i;
    EXPECT_VERIFIES(&env_i_i, code);
  }
}


TEST_F(WasmDecoderTest, Int8Const_fallthru2) {
  byte code[] = {kExprI8Const, 0, kExprI32Const, 1, 2, 3, 4};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, Int64Const) {
  byte code[] = {kExprI64Const, 0, 0, 0, 0, 0, 0, 0, 0};
  int64_t* ptr = reinterpret_cast<int64_t*>(code + 1);
  const int kInc = 4498211;
  for (int32_t i = kMinInt; i < kMaxInt - kInc; i = i + kInc) {
    *ptr = (static_cast<int64_t>(i) << 32) | i;
    EXPECT_VERIFIES(&env_l_l, code);
  }
}


TEST_F(WasmDecoderTest, Float32Const) {
  byte code[] = {kExprF32Const, 0, 0, 0, 0};
  float* ptr = reinterpret_cast<float*>(code + 1);
  for (int i = 0; i < 30; i++) {
    *ptr = i * -7.75f;
    EXPECT_VERIFIES(&env_f_ff, code);
  }
}


TEST_F(WasmDecoderTest, Float64Const) {
  byte code[] = {kExprF64Const, 0, 0, 0, 0, 0, 0, 0, 0};
  double* ptr = reinterpret_cast<double*>(code + 1);
  for (int i = 0; i < 30; i++) {
    *ptr = i * 33.45;
    EXPECT_VERIFIES(&env_d_dd, code);
  }
}


TEST_F(WasmDecoderTest, Int32Const_off_end) {
  byte code[] = {kExprI32Const, 0xaa, 0xbb, 0xcc, 0x44};

  for (int size = 1; size <= 4; size++) {
    Verify(kError, &env_i_i, code, code + size);
  }
}


TEST_F(WasmDecoderTest, GetLocal0_param) {
  EXPECT_VERIFIES(&env_i_i, kCodeGetLocal0);
}


TEST_F(WasmDecoderTest, GetLocal0_local) {
  FunctionEnv env;
  init_env(&env, sigs.i_v());
  env.AddLocals(kAstI32, 1);
  EXPECT_VERIFIES(&env, kCodeGetLocal0);
}


TEST_F(WasmDecoderTest, GetLocal0_param_n) {
  FunctionSig* array[] = {sigs.i_i(), sigs.i_ii(), sigs.i_iii()};

  for (size_t i = 0; i < arraysize(array); i++) {
    FunctionEnv env = CreateInt32FunctionEnv(array[i], 0);
    EXPECT_VERIFIES(&env, kCodeGetLocal0);
  }
}


TEST_F(WasmDecoderTest, GetLocalN_local) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprGetLocal, j};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(WasmDecoderTest, GetLocal0_fail_no_params) {
  FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), 0);

  EXPECT_FAILURE(&env, kCodeGetLocal0);
}


TEST_F(WasmDecoderTest, GetLocal1_fail_no_locals) {
  EXPECT_FAILURE(&env_i_i, kCodeGetLocal1);
}


TEST_F(WasmDecoderTest, GetLocal_off_end) {
  static const byte code[] = {kExprGetLocal};
  EXPECT_FAILURE(&env_i_i, code);
}


TEST_F(WasmDecoderTest, GetLocal_varint) {
  env_i_i.local_int32_count = 1000000000;
  env_i_i.total_locals += 1000000000;

  {
    static const byte code[] = {kExprGetLocal, 0xFF, 0x01};
    EXPECT_VERIFIES(&env_i_i, code);
    EXPECT_FAILURE(&env_i_f, code);
  }

  {
    static const byte code[] = {kExprGetLocal, 0xF0, 0x80, 0x01};
    EXPECT_VERIFIES(&env_i_i, code);
    EXPECT_FAILURE(&env_i_f, code);
  }

  {
    static const byte code[] = {kExprGetLocal, 0xF2, 0x81, 0x82, 0x01};
    EXPECT_VERIFIES(&env_i_i, code);
    EXPECT_FAILURE(&env_i_f, code);
  }

  {
    static const byte code[] = {kExprGetLocal, 0xF3, 0xA1, 0xB1, 0xC1, 0x01};
    EXPECT_VERIFIES(&env_i_i, code);
    EXPECT_FAILURE(&env_i_f, code);
  }
}


TEST_F(WasmDecoderTest, Binops_off_end) {
  byte code1[] = {0};  // [opcode]
  for (size_t i = 0; i < arraysize(kInt32BinopOpcodes); i++) {
    code1[0] = kInt32BinopOpcodes[i];
    EXPECT_FAILURE(&env_i_i, code1);
  }

  byte code3[] = {0, kExprGetLocal, 0};  // [opcode] [expr]
  for (size_t i = 0; i < arraysize(kInt32BinopOpcodes); i++) {
    code3[0] = kInt32BinopOpcodes[i];
    EXPECT_FAILURE(&env_i_i, code3);
  }

  byte code4[] = {0, kExprGetLocal, 0, 0};  // [opcode] [expr] [opcode]
  for (size_t i = 0; i < arraysize(kInt32BinopOpcodes); i++) {
    code4[0] = kInt32BinopOpcodes[i];
    code4[3] = kInt32BinopOpcodes[i];
    EXPECT_FAILURE(&env_i_i, code4);
  }
}


//===================================================================
//== Statements
//===================================================================
TEST_F(WasmDecoderTest, Nop) {
  static const byte code[] = {kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, SetLocal0_param) {
  static const byte code[] = {kExprSetLocal, 0, kExprI8Const, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, SetLocal0_local) {
  byte code[] = {kExprSetLocal, 0, kExprI8Const, 0};
  FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), 1);

  EXPECT_VERIFIES(&env, code);
}


TEST_F(WasmDecoderTest, SetLocalN_local) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprSetLocal, j, kExprI8Const, i};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(WasmDecoderTest, Switches0) {
  static const byte code[] = {kStmtSwitch, 0, kExprI8Const, 0};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 0, kExprI8Const, 0};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(WasmDecoderTest, Switches1) {
  static const byte code[] = {kStmtSwitch, 1, kExprI8Const, 0, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 1, kExprI8Const, 0, kStmtBlock,
                                0};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(WasmDecoderTest, Switches2) {
  static const byte code[] = {kStmtSwitch, 2, kExprI8Const, 0, kStmtNop,
                              kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 2, kExprI8Const, 0, kStmtBlock,
                                0, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(WasmDecoderTest, Switches4) {
  static const byte code[] = {kStmtSwitch, 4, kExprI8Const, 0, kStmtNop,
                              kStmtNop, kStmtNop, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 4, kExprI8Const, 0, kStmtBlock,
                                0, kStmtNop, kStmtNop, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(WasmDecoderTest, Switches4_break) {
  for (int i = 0; i < 4; i++) {
    byte code[] = {kStmtSwitch, 4, kExprI8Const, 0, kStmtBlock, 0, kStmtBlock,
                   0, kStmtBlock, 0, kStmtBlock, 0};
    code[4 + i * 2] = kStmtBreak;
    EXPECT_VERIFIES(&env_v_v, code);

    byte codenf[] = {kStmtSwitchNf, 4, kExprI8Const, 0, kStmtBlock, 0,
                     kStmtBlock, 0, kStmtBlock, 0, kStmtBlock, 0};
    codenf[4 + i * 2] = kStmtBreak;
    EXPECT_VERIFIES(&env_v_v, codenf);
  }
}


TEST_F(WasmDecoderTest, Block0) {
  static const byte code[] = {kStmtBlock, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Block0_fallthru1) {
  static const byte code[] = {kStmtBlock, 0, kStmtBlock, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Block1) {
  static const byte code[] = {kStmtBlock, 1, kExprSetLocal, 0, kExprI8Const,
                              0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, Block0_fallthru2) {
  static const byte code[] = {kStmtBlock, 0, kExprSetLocal, 0, kExprI8Const,
                              0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, Block2) {
  static const byte code[] = {kStmtBlock, 2,                         // --
                              kExprSetLocal, 0, kExprI8Const, 0,   // --
                              kExprSetLocal, 0, kExprI8Const, 0};  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, Block2_fallthru) {
  static const byte code[] = {kStmtBlock, 2,                        // --
                              kExprSetLocal, 0, kExprI8Const, 0,  // --
                              kExprSetLocal, 0, kExprI8Const, 0,  // --
                              kExprI8Const, 11};                  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(WasmDecoderTest, BlockN) {
  byte block[] = {kStmtBlock, 2};

  for (size_t i = 0; i < 10; i++) {
    size_t total = sizeof(block) + sizeof(kCodeSetLocal0) * i;
    byte* code = reinterpret_cast<byte*>(malloc(total));
    memcpy(code, block, sizeof(block));
    code[1] = static_cast<byte>(i);
    for (size_t j = 0; j < i; j++) {
      memcpy(code + sizeof(block) + j * sizeof(kCodeSetLocal0), kCodeSetLocal0,
             sizeof(kCodeSetLocal0));
    }
    Verify(kSuccess, &env_v_i, code, code + total);
    free(code);
  }
}


TEST_F(WasmDecoderTest, BlockN_off_end) {
  for (byte i = 2; i < 10; i++) {
    byte code[] = {kStmtBlock, i, kStmtContinue, 0};
    EXPECT_FAILURE(&env_v_v, code);
  }
}


TEST_F(WasmDecoderTest, Block1_break) {
  static const byte code[] = {kStmtBlock, 1, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Block2_break) {
  static const byte code[] = {kStmtBlock, 2, kStmtNop, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Block1_continue) {
  static const byte code[] = {kStmtBlock, 1, kStmtContinue, 0};
  EXPECT_FAILURE(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Block2_continue) {
  static const byte code[] = {kStmtBlock, 2, kStmtNop, kStmtContinue, 0};
  EXPECT_FAILURE(&env_v_v, code);
}


TEST_F(WasmDecoderTest, IfEmpty) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtNop};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, IfSet) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kExprSetLocal, 0,
                              kExprI8Const, 0};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, IfBlock1) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtBlock, 1,
                              kExprSetLocal, 0, kExprI8Const, 0};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, IfBlock2) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0,             // --
                              kStmtBlock, 2,                         // --
                              kExprSetLocal, 0, kExprI8Const, 0,   // --
                              kExprSetLocal, 0, kExprI8Const, 0};  // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, IfThenEmpty) {
  static const byte code[] = {kStmtIfThen, kExprGetLocal, 0, kStmtNop,
                              kStmtNop};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, IfThenSet) {
  static const byte code[] = {kStmtIfThen, kExprGetLocal, 0,         // --
                              kExprSetLocal, 0, kExprI8Const, 0,   // --
                              kExprSetLocal, 0, kExprI8Const, 1};  // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, Loop0) {
  static const byte code[] = {kStmtLoop, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Loop1) {
  static const byte code[] = {kStmtLoop, 1, kExprSetLocal, 0, kExprI8Const,
                              0};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, Loop2) {
  static const byte code[] = {kStmtLoop, 2,                          // --
                              kExprSetLocal, 0, kExprI8Const, 0,   // --
                              kExprSetLocal, 0, kExprI8Const, 0};  // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, Loop1_continue) {
  static const byte code[] = {kStmtLoop, 1, kStmtContinue, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Loop1_break) {
  static const byte code[] = {kStmtLoop, 1, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(WasmDecoderTest, Loop2_continue) {
  static const byte code[] = {kStmtLoop, 2,                         // --
                              kExprSetLocal, 0, kExprI8Const, 0,  // --
                              kStmtContinue, 0};                    // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, Loop2_break) {
  static const byte code[] = {kStmtLoop, 2,                         // --
                              kExprSetLocal, 0, kExprI8Const, 0,  // --
                              kStmtBreak, 0};                       // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(WasmDecoderTest, ReturnVoid) {
  static const byte code[] = {kStmtReturn};
  EXPECT_VERIFIES(&env_v_v, code);
  EXPECT_FAILURE(&env_i_i, code);
  EXPECT_FAILURE(&env_i_f, code);
}


TEST_F(WasmDecoderTest, UnreachableCode1) {
  EXPECT_FAILURE_INLINE(&env_v_v, kStmtReturn, kStmtNop);
}


TEST_F(WasmDecoderTest, UnreachableCode2) {
  EXPECT_FAILURE_INLINE(&env_i_i, kStmtReturn, kExprI8Const, 0, kStmtNop);
}


TEST_F(WasmDecoderTest, UnreachableCode3) {
  EXPECT_FAILURE_INLINE(&env_i_i, kStmtLoop, 0, kExprI8Const, 0, kStmtNop);
}


TEST_F(WasmDecoderTest, Unreachable4) {
  EXPECT_FAILURE_INLINE(
      &env_i_i,
      WASM_BLOCK(2, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(1)),
                              WASM_SET_LOCAL(0, WASM_I8(99))),
                 /*unreachable*/ WASM_RETURN(WASM_GET_LOCAL(0))),
      WASM_I8(43));
}


TEST_F(WasmDecoderTest, Codeiness) {
  VERIFY(kStmtLoop, 2,                         // --
         kExprSetLocal, 0, kExprI8Const, 0,  // --
         kStmtBreak, 0);                       // --
}


TEST_F(WasmDecoderTest, Ternary1) {
  VERIFY(kExprTernary, kExprGetLocal, 0, kExprI8Const, 0, kExprI8Const, 1);
  VERIFY(kExprTernary, kExprGetLocal, 0, kExprGetLocal, 0, kExprGetLocal, 0);
  VERIFY(kExprTernary, kExprGetLocal, 0, kExprI32Add, kExprGetLocal, 0,
         kExprGetLocal, 0, kExprI8Const, 1);
}


TEST_F(WasmDecoderTest, Comma1) {
  VERIFY(kExprComma, kExprI8Const, 0, kExprI8Const, 1);
  VERIFY(kExprComma, kExprGetLocal, 0, kExprGetLocal, 0);
  VERIFY(kExprComma, kExprI32Add, kExprGetLocal, 0, kExprGetLocal, 0,
         kExprI8Const, 1);
}


TEST_F(WasmDecoderTest, Ternary_off_end) {
  static const byte kCode[] = {kExprTernary, kExprGetLocal, 0, kExprGetLocal, 0,
                               kExprGetLocal, 0};
  for (size_t len = 1; len < arraysize(kCode); len++) {
    Verify(kError, &env_i_i, kCode, kCode + len);
  }
}


TEST_F(WasmDecoderTest, Ternary_type) {
  {
    // float|double ? 1 : 2
    static const byte kCode[] = {kExprTernary, kExprGetLocal, 0, kExprI8Const,
                                 1, kExprI8Const, 2};
    EXPECT_FAILURE(&env_i_f, kCode);
    EXPECT_FAILURE(&env_i_d, kCode);
  }
  {
    // 1 ? float|double : 2
    static const byte kCode[] = {kExprTernary, kExprI8Const, 1, kExprGetLocal,
                                 0, kExprI8Const, 2};
    EXPECT_FAILURE(&env_i_f, kCode);
    EXPECT_FAILURE(&env_i_d, kCode);
  }
  {
    // stmt ? 0 : 1
    static const byte kCode[] = {kExprTernary, kStmtNop, kExprI8Const, 0,
                                 kExprI8Const, 1};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
  {
    // 0 ? stmt : 1
    static const byte kCode[] = {kExprTernary, kExprI8Const, 0, kStmtNop,
                                 kExprI8Const, 1};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
  {
    // 0 ? 1 : stmt
    static const byte kCode[] = {kExprTernary, kExprI8Const, 0,
                                 kExprI8Const, 1, 0, kStmtBlock};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
}


TEST_F(WasmDecoderTest, Int64Local_param) {
  EXPECT_VERIFIES(&env_l_l, kCodeGetLocal0);
}


TEST_F(WasmDecoderTest, Int64Locals) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env;
    init_env(&env, sigs.l_v());
    env.AddLocals(kAstI64, i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprGetLocal, j};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(WasmDecoderTest, Int32Binops) {
  TestBinop(kExprI32Add, sigs.i_ii());
  TestBinop(kExprI32Sub, sigs.i_ii());
  TestBinop(kExprI32Mul, sigs.i_ii());
  TestBinop(kExprI32DivS, sigs.i_ii());
  TestBinop(kExprI32DivU, sigs.i_ii());
  TestBinop(kExprI32RemS, sigs.i_ii());
  TestBinop(kExprI32RemU, sigs.i_ii());
  TestBinop(kExprI32And, sigs.i_ii());
  TestBinop(kExprI32Ior, sigs.i_ii());
  TestBinop(kExprI32Xor, sigs.i_ii());
  TestBinop(kExprI32Shl, sigs.i_ii());
  TestBinop(kExprI32ShrU, sigs.i_ii());
  TestBinop(kExprI32ShrS, sigs.i_ii());
  TestBinop(kExprI32Eq, sigs.i_ii());
  TestBinop(kExprI32LtS, sigs.i_ii());
  TestBinop(kExprI32LeS, sigs.i_ii());
  TestBinop(kExprI32LtU, sigs.i_ii());
  TestBinop(kExprI32LeU, sigs.i_ii());
}


TEST_F(WasmDecoderTest, DoubleBinops) {
  TestBinop(kExprF64Add, sigs.d_dd());
  TestBinop(kExprF64Sub, sigs.d_dd());
  TestBinop(kExprF64Mul, sigs.d_dd());
  TestBinop(kExprF64Div, sigs.d_dd());

  TestBinop(kExprF64Eq, sigs.i_dd());
  TestBinop(kExprF64Lt, sigs.i_dd());
  TestBinop(kExprF64Le, sigs.i_dd());
}


TEST_F(WasmDecoderTest, FloatBinops) {
  TestBinop(kExprF32Add, sigs.f_ff());
  TestBinop(kExprF32Sub, sigs.f_ff());
  TestBinop(kExprF32Mul, sigs.f_ff());
  TestBinop(kExprF32Div, sigs.f_ff());

  TestBinop(kExprF32Eq, sigs.i_ff());
  TestBinop(kExprF32Lt, sigs.i_ff());
  TestBinop(kExprF32Le, sigs.i_ff());
}


TEST_F(WasmDecoderTest, TypeConversions) {
  TestUnop(kExprI32SConvertF32, kAstI32, kAstF32);
  TestUnop(kExprI32SConvertF64, kAstI32, kAstF64);
  TestUnop(kExprI32UConvertF32, kAstI32, kAstF32);
  TestUnop(kExprI32UConvertF64, kAstI32, kAstF64);
  TestUnop(kExprF64SConvertI32, kAstF64, kAstI32);
  TestUnop(kExprF64UConvertI32, kAstF64, kAstI32);
  TestUnop(kExprF64ConvertF32, kAstF64, kAstF32);
  TestUnop(kExprF32SConvertI32, kAstF32, kAstI32);
  TestUnop(kExprF32UConvertI32, kAstF32, kAstI32);
  TestUnop(kExprF32ConvertF64, kAstF32, kAstF64);
}


TEST_F(WasmDecoderTest, MacrosStmt) {
  VERIFY(WASM_SET_LOCAL(0, WASM_I32(87348)));
  VERIFY(WASM_STORE_MEM(kMemI32, WASM_I8(24), WASM_I8(40)));
  VERIFY(WASM_IF(WASM_GET_LOCAL(0), WASM_NOP));
  VERIFY(WASM_IF_THEN(WASM_GET_LOCAL(0), WASM_NOP, WASM_NOP));
  VERIFY(WASM_NOP);
  VERIFY(WASM_BLOCK(1, WASM_NOP));
  VERIFY(WASM_LOOP(1, WASM_NOP));
  VERIFY(WASM_LOOP(1, WASM_BREAK(0)));
  VERIFY(WASM_LOOP(1, WASM_CONTINUE(0)));
}

TEST_F(WasmDecoderTest, MacrosReturn) {
  EXPECT_VERIFIES_INLINE(&env_i_i, WASM_RETURN(WASM_ZERO));
  EXPECT_VERIFIES_INLINE(&env_l_l, WASM_RETURN(WASM_I64(0)));
  EXPECT_VERIFIES_INLINE(&env_f_ff, WASM_RETURN(WASM_F32(0.0)));
  EXPECT_VERIFIES_INLINE(&env_d_dd, WASM_RETURN(WASM_F64(0.0)));

  static LocalType kIntTypes[] = {kAstI32, kAstI32};
  FunctionSig sig_ii_v(2, 0, kIntTypes);
  FunctionEnv env_ii_v;
  init_env(&env_ii_v, &sig_ii_v);

  EXPECT_VERIFIES_INLINE(&env_ii_v, WASM_RETURN(WASM_ZERO, WASM_ZERO));
}


TEST_F(WasmDecoderTest, MacrosVariadic) {
  VERIFY(WASM_BLOCK(2, WASM_NOP, WASM_NOP));
  VERIFY(WASM_BLOCK(3, WASM_NOP, WASM_NOP, WASM_NOP));
  VERIFY(WASM_LOOP(2, WASM_NOP, WASM_NOP));
  VERIFY(WASM_LOOP(3, WASM_NOP, WASM_NOP, WASM_NOP));
}


TEST_F(WasmDecoderTest, MacrosNestedBlocks) {
  VERIFY(WASM_BLOCK(2, WASM_NOP, WASM_BLOCK(2, WASM_NOP, WASM_NOP)));
  VERIFY(WASM_BLOCK(3, WASM_NOP,                          // --
                    WASM_BLOCK(2, WASM_NOP, WASM_NOP),    // --
                    WASM_BLOCK(2, WASM_NOP, WASM_NOP)));  // --
  VERIFY(WASM_BLOCK(1, WASM_BLOCK(1, WASM_BLOCK(2, WASM_NOP, WASM_NOP))));
}


TEST_F(WasmDecoderTest, MultipleReturn) {
  static LocalType kIntTypes5[] = {kAstI32, kAstI32, kAstI32, kAstI32,
                                   kAstI32};
  FunctionSig sig_ii_v(2, 0, kIntTypes5);
  FunctionEnv env_ii_v;
  init_env(&env_ii_v, &sig_ii_v);
  EXPECT_VERIFIES_INLINE(&env_ii_v, WASM_RETURN(WASM_ZERO, WASM_ONE));
  EXPECT_FAILURE_INLINE(&env_ii_v, WASM_RETURN(WASM_ZERO));

  FunctionSig sig_iii_v(3, 0, kIntTypes5);
  FunctionEnv env_iii_v;
  init_env(&env_iii_v, &sig_iii_v);
  EXPECT_VERIFIES_INLINE(&env_iii_v,
                         WASM_RETURN(WASM_ZERO, WASM_ONE, WASM_I8(44)));
  EXPECT_FAILURE_INLINE(&env_iii_v, WASM_RETURN(WASM_ZERO, WASM_ONE));
}


TEST_F(WasmDecoderTest, MultipleReturn_fallthru) {
  static LocalType kIntTypes5[] = {kAstI32, kAstI32, kAstI32, kAstI32,
                                   kAstI32};
  FunctionSig sig_ii_v(2, 0, kIntTypes5);
  FunctionEnv env_ii_v;
  init_env(&env_ii_v, &sig_ii_v);

  EXPECT_VERIFIES_INLINE(&env_ii_v, WASM_ZERO, WASM_ONE);
  EXPECT_FAILURE_INLINE(&env_ii_v, WASM_ZERO);

  FunctionSig sig_iii_v(3, 0, kIntTypes5);
  FunctionEnv env_iii_v;
  init_env(&env_iii_v, &sig_iii_v);
  EXPECT_VERIFIES_INLINE(&env_iii_v, WASM_ZERO, WASM_ONE, WASM_I8(44));
  EXPECT_FAILURE_INLINE(&env_iii_v, WASM_ZERO, WASM_ONE);
}


TEST_F(WasmDecoderTest, MacrosInt32) {
  VERIFY(WASM_I32_ADD(WASM_GET_LOCAL(0), WASM_I8(12)));
  VERIFY(WASM_I32_SUB(WASM_GET_LOCAL(0), WASM_I8(13)));
  VERIFY(WASM_I32_MUL(WASM_GET_LOCAL(0), WASM_I8(14)));
  VERIFY(WASM_I32_DIVS(WASM_GET_LOCAL(0), WASM_I8(15)));
  VERIFY(WASM_I32_DIVU(WASM_GET_LOCAL(0), WASM_I8(16)));
  VERIFY(WASM_I32_REMS(WASM_GET_LOCAL(0), WASM_I8(17)));
  VERIFY(WASM_I32_REMU(WASM_GET_LOCAL(0), WASM_I8(18)));
  VERIFY(WASM_I32_AND(WASM_GET_LOCAL(0), WASM_I8(19)));
  VERIFY(WASM_I32_IOR(WASM_GET_LOCAL(0), WASM_I8(20)));
  VERIFY(WASM_I32_XOR(WASM_GET_LOCAL(0), WASM_I8(21)));
  VERIFY(WASM_I32_SHL(WASM_GET_LOCAL(0), WASM_I8(22)));
  VERIFY(WASM_I32_SHR(WASM_GET_LOCAL(0), WASM_I8(23)));
  VERIFY(WASM_I32_SAR(WASM_GET_LOCAL(0), WASM_I8(24)));
  VERIFY(WASM_I32_EQ(WASM_GET_LOCAL(0), WASM_I8(25)));
  VERIFY(WASM_I32_NE(WASM_GET_LOCAL(0), WASM_I8(25)));

  VERIFY(WASM_I32_LTS(WASM_GET_LOCAL(0), WASM_I8(26)));
  VERIFY(WASM_I32_LES(WASM_GET_LOCAL(0), WASM_I8(27)));
  VERIFY(WASM_I32_LTU(WASM_GET_LOCAL(0), WASM_I8(28)));
  VERIFY(WASM_I32_LEU(WASM_GET_LOCAL(0), WASM_I8(29)));

  VERIFY(WASM_I32_GTS(WASM_GET_LOCAL(0), WASM_I8(26)));
  VERIFY(WASM_I32_GES(WASM_GET_LOCAL(0), WASM_I8(27)));
  VERIFY(WASM_I32_GTU(WASM_GET_LOCAL(0), WASM_I8(28)));
  VERIFY(WASM_I32_GEU(WASM_GET_LOCAL(0), WASM_I8(29)));
}


TEST_F(WasmDecoderTest, MacrosInt64) {
  FunctionEnv env_i_ll;
  FunctionEnv env_l_ll;
  init_env(&env_i_ll, sigs.i_ll());
  init_env(&env_l_ll, sigs.l_ll());

#define VERIFY_L_LL(...) EXPECT_VERIFIES_INLINE(&env_l_ll, __VA_ARGS__)
#define VERIFY_I_LL(...) EXPECT_VERIFIES_INLINE(&env_i_ll, __VA_ARGS__)

  VERIFY_L_LL(WASM_I64_ADD(WASM_GET_LOCAL(0), WASM_I64(12)));
  VERIFY_L_LL(WASM_I64_SUB(WASM_GET_LOCAL(0), WASM_I64(13)));
  VERIFY_L_LL(WASM_I64_MUL(WASM_GET_LOCAL(0), WASM_I64(14)));
  VERIFY_L_LL(WASM_I64_DIVS(WASM_GET_LOCAL(0), WASM_I64(15)));
  VERIFY_L_LL(WASM_I64_DIVU(WASM_GET_LOCAL(0), WASM_I64(16)));
  VERIFY_L_LL(WASM_I64_REMS(WASM_GET_LOCAL(0), WASM_I64(17)));
  VERIFY_L_LL(WASM_I64_REMU(WASM_GET_LOCAL(0), WASM_I64(18)));
  VERIFY_L_LL(WASM_I64_AND(WASM_GET_LOCAL(0), WASM_I64(19)));
  VERIFY_L_LL(WASM_I64_IOR(WASM_GET_LOCAL(0), WASM_I64(20)));
  VERIFY_L_LL(WASM_I64_XOR(WASM_GET_LOCAL(0), WASM_I64(21)));

  VERIFY_L_LL(WASM_I64_SHL(WASM_GET_LOCAL(0), WASM_I64(22)));
  VERIFY_L_LL(WASM_I64_SHR(WASM_GET_LOCAL(0), WASM_I64(23)));
  VERIFY_L_LL(WASM_I64_SAR(WASM_GET_LOCAL(0), WASM_I64(24)));

  VERIFY_I_LL(WASM_I64_LTS(WASM_GET_LOCAL(0), WASM_I64(26)));
  VERIFY_I_LL(WASM_I64_LES(WASM_GET_LOCAL(0), WASM_I64(27)));
  VERIFY_I_LL(WASM_I64_LTU(WASM_GET_LOCAL(0), WASM_I64(28)));
  VERIFY_I_LL(WASM_I64_LEU(WASM_GET_LOCAL(0), WASM_I64(29)));

  VERIFY_I_LL(WASM_I64_GTS(WASM_GET_LOCAL(0), WASM_I64(26)));
  VERIFY_I_LL(WASM_I64_GES(WASM_GET_LOCAL(0), WASM_I64(27)));
  VERIFY_I_LL(WASM_I64_GTU(WASM_GET_LOCAL(0), WASM_I64(28)));
  VERIFY_I_LL(WASM_I64_GEU(WASM_GET_LOCAL(0), WASM_I64(29)));

  VERIFY_I_LL(WASM_I64_EQ(WASM_GET_LOCAL(0), WASM_I64(25)));
  VERIFY_I_LL(WASM_I64_NE(WASM_GET_LOCAL(0), WASM_I64(25)));
}


TEST_F(WasmDecoderTest, AllSimpleExpressions) {
// Test all simple expressions which are described by a signature.
#define DECODE_TEST(name, opcode, sig)                      \
  {                                                         \
    FunctionSig* sig = WasmOpcodes::Signature(kExpr##name); \
    if (sig->parameter_count() == 1) {                      \
      TestUnop(kExpr##name, sig);                           \
    } else {                                                \
      TestBinop(kExpr##name, sig);                          \
    }                                                       \
  }

  FOREACH_SIMPLE_EXPR_OPCODE(DECODE_TEST);

#undef DECODE_TEST
}


TEST_F(WasmDecoderTest, AllLoadMemCombinations) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType local_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kMemTypes); j++) {
      MemType mem_type = kMemTypes[j];
      byte code[] = {
          kStmtReturn, WasmOpcodes::LoadStoreOpcodeOf(mem_type, false),
          WasmOpcodes::LoadStoreAccessOf(mem_type), kExprI8Const, 0};
      FunctionEnv env;
      FunctionSig sig(1, 0, &local_type);
      init_env(&env, &sig);
      if (local_type == WasmOpcodes::LocalTypeFor(mem_type)) {
        EXPECT_VERIFIES(&env, code);
      } else {
        EXPECT_FAILURE(&env, code);
      }
    }
  }
}


TEST_F(WasmDecoderTest, AllStoreMemCombinations) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType local_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kMemTypes); j++) {
      MemType mem_type = kMemTypes[j];
      byte code[] = {WasmOpcodes::LoadStoreOpcodeOf(mem_type, true),
                     WasmOpcodes::LoadStoreAccessOf(mem_type), kExprI8Const,
                     0, kExprGetLocal, 0};
      FunctionEnv env;
      FunctionSig sig(0, 1, &local_type);
      init_env(&env, &sig);
      if (local_type == WasmOpcodes::LocalTypeFor(mem_type)) {
        EXPECT_VERIFIES(&env, code);
      } else {
        EXPECT_FAILURE(&env, code);
      }
    }
  }
}


TEST_F(WasmDecoderTest, LoadMemHigh) {
  WasmOpcode opcodes[] = {kExprI32LoadMemH, kExprI64LoadMemH,
                          kExprF32LoadMemH, kExprF64LoadMemH};
  LocalType local_types[] = {kAstI32, kAstI64, kAstF32, kAstF64};
  MemType mem_types[] = {kMemI32, kMemI64, kMemF32, kMemF64};
  // Check that all Expr<T>LoadMemH instructions take an int64 index.
  for (size_t i = 0; i < arraysize(local_types); i++) {
    LocalType type = local_types[i];
    LocalType sig_types[] = {type, kAstI64};
    FunctionSig sig(1, 1, sig_types);
    FunctionEnv env;
    init_env(&env, &sig);
    byte code[] = {static_cast<byte>(opcodes[i]),
                   WasmOpcodes::LoadStoreAccessOf(mem_types[i]), kExprGetLocal,
                   0};
    EXPECT_VERIFIES(&env, code);
  }
}


TEST_F(WasmDecoderTest, StoreMemHigh) {
  WasmOpcode opcodes[] = {kExprI32StoreMemH, kExprI64StoreMemH,
                          kExprF32StoreMemH, kExprF64LoadMemH};
  LocalType local_types[] = {kAstI32, kAstI64, kAstF32, kAstF64};
  MemType mem_types[] = {kMemI32, kMemI64, kMemF32, kMemF64};
  // Check that all Expr<T>StoreMemH instructions take an int64 index.
  for (size_t i = 0; i < arraysize(local_types); i++) {
    LocalType type = local_types[i];
    LocalType sig_types[] = {type, kAstI64, type};
    FunctionSig sig(1, 2, sig_types);
    FunctionEnv env;
    init_env(&env, &sig);
    byte code[] = {static_cast<byte>(opcodes[i]),
                   WasmOpcodes::LoadStoreAccessOf(mem_types[i]), kExprGetLocal,
                   0, kExprGetLocal, 1};
    EXPECT_VERIFIES(&env, code);
  }
}


namespace {
// A helper for tests that require a module environment for functions and
// globals.
class TestModuleEnv : public ModuleEnv {
 public:
  TestModuleEnv() {
    mem_start = 0;
    mem_end = 0;
    module = &mod;
    linker = nullptr;
    function_code = nullptr;
    mod.functions = &functions;
    mod.globals = &globals;
  }
  void AddFunction(FunctionSig* sig) {
    functions.push_back({sig, 0, 0, 0, 0, 0, 0, 0, false, false});
  }
  void AddGlobal(MemType mem_type) {
    globals.push_back({0, mem_type, 0, false});
  }

 private:
  WasmModule mod;
  std::vector<WasmFunction> functions;
  std::vector<WasmGlobal> globals;
};
}

TEST_F(WasmDecoderTest, SimpleCalls) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_v());
  module_env.AddFunction(sigs.i_i());
  module_env.AddFunction(sigs.i_ii());

  EXPECT_VERIFIES_INLINE(env, WASM_CALL_FUNCTION(0));
  EXPECT_VERIFIES_INLINE(env, WASM_CALL_FUNCTION(1, WASM_I8(27)));
  EXPECT_VERIFIES_INLINE(env,
                         WASM_CALL_FUNCTION(2, WASM_I8(37), WASM_I8(77)));
}


TEST_F(WasmDecoderTest, CallsWithTooFewArguments) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_i());
  module_env.AddFunction(sigs.i_ii());
  module_env.AddFunction(sigs.f_ff());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION0(0));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_ZERO));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(2, WASM_GET_LOCAL(0)));
}


TEST_F(WasmDecoderTest, CallsWithSpilloverArgs) {
  static LocalType a_i_ff[] = {kAstI32, kAstF32, kAstF32};
  FunctionSig sig_i_ff(1, 2, a_i_ff);
  FunctionEnv env_i_ff;
  init_env(&env_i_ff, &sig_i_ff);

  TestModuleEnv module_env;
  env_i_ff.module = &module_env;
  env_i_i.module = &module_env;
  env_f_ff.module = &module_env;

  module_env.AddFunction(&sig_i_ff);

  EXPECT_VERIFIES_INLINE(
      &env_i_i, WASM_CALL_FUNCTION(0, WASM_F32(0.1), WASM_F32(0.1)));

  EXPECT_VERIFIES_INLINE(
      &env_i_ff, WASM_CALL_FUNCTION(0, WASM_F32(0.1), WASM_F32(0.1)));

  EXPECT_FAILURE_INLINE(
      &env_f_ff, WASM_CALL_FUNCTION(0, WASM_F32(0.1), WASM_F32(0.1)));

  EXPECT_FAILURE_INLINE(
      &env_i_i, WASM_CALL_FUNCTION(0, WASM_F32(0.1), WASM_F32(0.1),
                                   WASM_F32(0.2)));

  EXPECT_VERIFIES_INLINE(
      &env_f_ff, WASM_CALL_FUNCTION(0, WASM_F32(0.1), WASM_F32(0.1),
                                    WASM_F32(11)));
}


TEST_F(WasmDecoderTest, CallsWithMismatchedSigs2) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_i());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_I64(17)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_F32(17.1)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_F64(17.1)));
}


TEST_F(WasmDecoderTest, CallsWithMismatchedSigs3) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_f());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_I8(17)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_I64(27)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_F64(37.2)));

  module_env.AddFunction(sigs.i_d());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_I8(16)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_I64(16)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_F32(17.6)));
}


TEST_F(WasmDecoderTest, Int32Globals) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemI8);
  module_env.AddGlobal(kMemU8);
  module_env.AddGlobal(kMemI16);
  module_env.AddGlobal(kMemU16);
  module_env.AddGlobal(kMemI32);
  module_env.AddGlobal(kMemU32);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(1)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(2)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(3)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(4)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(5)));

  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(1, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(2, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(3, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(4, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(5, WASM_GET_LOCAL(0)));
}


TEST_F(WasmDecoderTest, Int32Globals_fail) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemI64);
  module_env.AddGlobal(kMemU64);
  module_env.AddGlobal(kMemF32);
  module_env.AddGlobal(kMemF64);

  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(1)));
  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(2)));
  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(3)));

  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(1, WASM_GET_LOCAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(2, WASM_GET_LOCAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(3, WASM_GET_LOCAL(0)));
}


TEST_F(WasmDecoderTest, Int64Globals) {
  FunctionEnv* env = &env_l_l;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemI64);
  module_env.AddGlobal(kMemU64);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(1)));

  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(1, WASM_GET_LOCAL(0)));
}


TEST_F(WasmDecoderTest, Float32Globals) {
  FunctionEnv env_f_ff;
  FunctionEnv* env = &env_f_ff;
  init_env(env, sigs.f_ff());
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemF32);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
}


TEST_F(WasmDecoderTest, Float64Globals) {
  FunctionEnv env_d_dd;
  FunctionEnv* env = &env_d_dd;
  init_env(env, sigs.d_dd());
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemF64);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
}


TEST_F(WasmDecoderTest, AllLoadGlobalCombinations) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType local_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kMemTypes); j++) {
      MemType mem_type = kMemTypes[j];
      FunctionEnv env;
      FunctionSig sig(1, 0, &local_type);
      TestModuleEnv module_env;
      init_env(&env, &sig);
      env.module = &module_env;
      module_env.AddGlobal(mem_type);
      if (local_type == WasmOpcodes::LocalTypeFor(mem_type)) {
        EXPECT_VERIFIES_INLINE(&env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
      } else {
        EXPECT_FAILURE_INLINE(&env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
      }
    }
  }
}


TEST_F(WasmDecoderTest, AllStoreGlobalCombinations) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType local_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kMemTypes); j++) {
      MemType mem_type = kMemTypes[j];
      FunctionEnv env;
      FunctionSig sig(0, 1, &local_type);
      TestModuleEnv module_env;
      init_env(&env, &sig);
      env.module = &module_env;
      module_env.AddGlobal(mem_type);
      if (local_type == WasmOpcodes::LocalTypeFor(mem_type)) {
        EXPECT_VERIFIES_INLINE(&env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
      } else {
        EXPECT_FAILURE_INLINE(&env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
      }
    }
  }
}


TEST_F(WasmDecoderTest, BreakNesting1) {
  for (int i = 0; i < 5; i++) {
    // (block[2] (loop[2] (if (get p) break[N]) (set p 1)) (return p))
    byte code[] = {
        WASM_BLOCK(2, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(i)),
                                WASM_SET_LOCAL(0, WASM_I8(1))),
                   WASM_RETURN(WASM_GET_LOCAL(0)))};
    if (i == 0) {
      EXPECT_VERIFIES(&env_i_i, code);
    } else {
      EXPECT_FAILURE(&env_i_i, code);
    }
  }
}


TEST_F(WasmDecoderTest, BreakNesting2) {
  env_v_v.AddLocals(kAstI32, 1);
  for (int i = 0; i < 5; i++) {
    // (block[2] (loop[2] (if (get p) break[N]) (set p 1)) (return p)) (11)
    byte code[] = {
        WASM_BLOCK(1, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(i)),
                                WASM_SET_LOCAL(0, WASM_I8(1)))),
        WASM_I8(11)};
    if (i < 2) {
      EXPECT_VERIFIES(&env_v_v, code);
    } else {
      EXPECT_FAILURE(&env_v_v, code);
    }
  }
}


TEST_F(WasmDecoderTest, BreakNesting3) {
  env_v_v.AddLocals(kAstI32, 1);
  for (int i = 0; i < 5; i++) {
    // (block[1] (loop[1] (block[1] (if (get p) break[N])
    byte code[] = {WASM_BLOCK(
        1, WASM_LOOP(
               1, WASM_BLOCK(1, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(i)))))};
    if (i < 3) {
      EXPECT_VERIFIES(&env_v_v, code);
    } else {
      EXPECT_FAILURE(&env_v_v, code);
    }
  }
}


TEST_F(WasmDecoderTest, BreakNesting_6_levels) {
  for (int mask = 0; mask < 64; mask++) {
    for (int i = 0; i < 12; i++) {
      byte code[] = {
          kStmtBlock, 1,                    // --
          kStmtBlock, 1,                    // --
          kStmtBlock, 1,                    // --
          kStmtBlock, 1,                    // --
          kStmtBlock, 1,                    // --
          kStmtBlock, 1,                    // --
          kStmtBreak, static_cast<byte>(i)  // --
      };

      for (int l = 0; l < 6; l++) {
        if (mask & (1 << l)) code[l * 2] = kStmtLoop;
      }

      if (i < 6) {
        EXPECT_VERIFIES(&env_v_v, code);
      } else {
        EXPECT_FAILURE(&env_v_v, code);
      }
    }
  }
}


TEST_F(WasmDecoderTest, ContinueNesting_6_levels) {
  for (int mask = 0; mask < 64; mask++) {
    for (int i = 0; i < 12; i++) {
      byte code[] = {
          kStmtBlock, 1,                       // --
          kStmtBlock, 1,                       // --
          kStmtBlock, 1,                       // --
          kStmtBlock, 1,                       // --
          kStmtBlock, 1,                       // --
          kStmtBlock, 1,                       // --
          kStmtContinue, static_cast<byte>(i)  // --
      };

      for (int l = 0; l < 6; l++) {
        if (mask & (1 << l)) code[10 - l * 2] = kStmtLoop;
      }

      if (i < 6 && (mask & (1 << i))) {
        EXPECT_VERIFIES(&env_v_v, code);
      } else {
        EXPECT_FAILURE(&env_v_v, code);
      }
    }
  }
}
}
}
}
