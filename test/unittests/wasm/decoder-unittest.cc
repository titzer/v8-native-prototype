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
static const byte kCodeSetLocal0[] = {kExprSetLocal, 0, kExprInt8Const, 0};

static const LocalType kLocalTypes[] = {kAstInt32, kAstInt64, kAstFloat32,
                                        kAstFloat64};
static const MemType kMemTypes[] = {
    kMemInt8,   kMemUint8, kMemInt16,  kMemUint16,  kMemInt32,
    kMemUint32, kMemInt64, kMemUint64, kMemFloat32, kMemFloat64};

static const WasmOpcode kInt32BinopOpcodes[] = {
    kExprInt32Add,  kExprInt32Sub,  kExprInt32Mul,  kExprInt32SDiv,
    kExprInt32UDiv, kExprInt32SRem, kExprInt32URem, kExprInt32And,
    kExprInt32Ior,  kExprInt32Xor,  kExprInt32Shl,  kExprInt32Shr,
    kExprInt32Sar,  kExprInt32Eq,   kExprInt32Slt,  kExprInt32Sle,
    kExprInt32Ult,  kExprInt32Ule};


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


class DecoderTest : public TestWithZone {
 public:
  DecoderTest() : TestWithZone(), sigs() {
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


TEST_F(DecoderTest, Int8Const) {
  byte code[] = {kExprInt8Const, 0};
  for (int i = -128; i < 128; i++) {
    code[1] = static_cast<byte>(i);
    EXPECT_VERIFIES(&env_i_i, code);
  }
}


TEST_F(DecoderTest, Int8Const_fallthru) {
  byte code[] = {kExprInt8Const, 0, kExprInt8Const, 1};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Int32Const) {
  byte code[] = {kExprInt32Const, 0, 0, 0, 0};
  int32_t* ptr = reinterpret_cast<int32_t*>(code + 1);
  const int kInc = 4498211;
  for (int32_t i = kMinInt; i < kMaxInt - kInc; i = i + kInc) {
    *ptr = i;
    EXPECT_VERIFIES(&env_i_i, code);
  }
}


TEST_F(DecoderTest, Int8Const_fallthru2) {
  byte code[] = {kExprInt8Const, 0, kExprInt32Const, 1, 2, 3, 4};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Int64Const) {
  byte code[] = {kExprInt64Const, 0, 0, 0, 0, 0, 0, 0, 0};
  int64_t* ptr = reinterpret_cast<int64_t*>(code + 1);
  const int kInc = 4498211;
  for (int32_t i = kMinInt; i < kMaxInt - kInc; i = i + kInc) {
    *ptr = (static_cast<int64_t>(i) << 32) | i;
    EXPECT_VERIFIES(&env_l_l, code);
  }
}


TEST_F(DecoderTest, Float32Const) {
  byte code[] = {kExprFloat32Const, 0, 0, 0, 0};
  float* ptr = reinterpret_cast<float*>(code + 1);
  for (int i = 0; i < 30; i++) {
    *ptr = i * -7.75f;
    EXPECT_VERIFIES(&env_f_ff, code);
  }
}


TEST_F(DecoderTest, Float64Const) {
  byte code[] = {kExprFloat64Const, 0, 0, 0, 0, 0, 0, 0, 0};
  double* ptr = reinterpret_cast<double*>(code + 1);
  for (int i = 0; i < 30; i++) {
    *ptr = i * 33.45;
    EXPECT_VERIFIES(&env_d_dd, code);
  }
}


TEST_F(DecoderTest, Int32Const_off_end) {
  byte code[] = {kExprInt32Const, 0xaa, 0xbb, 0xcc, 0x44};

  for (int size = 1; size <= 4; size++) {
    Verify(kError, &env_i_i, code, code + size);
  }
}


TEST_F(DecoderTest, GetLocal0_param) {
  EXPECT_VERIFIES(&env_i_i, kCodeGetLocal0);
}


TEST_F(DecoderTest, GetLocal0_local) {
  FunctionEnv env;
  init_env(&env, sigs.i_v());
  env.AddLocals(kAstInt32, 1);
  EXPECT_VERIFIES(&env, kCodeGetLocal0);
}


TEST_F(DecoderTest, GetLocal0_param_n) {
  FunctionSig* array[] = {sigs.i_i(), sigs.i_ii(), sigs.i_iii()};

  for (size_t i = 0; i < arraysize(array); i++) {
    FunctionEnv env = CreateInt32FunctionEnv(array[i], 0);
    EXPECT_VERIFIES(&env, kCodeGetLocal0);
  }
}


TEST_F(DecoderTest, GetLocalN_local) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprGetLocal, j};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(DecoderTest, GetLocal0_fail_no_params) {
  FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), 0);

  EXPECT_FAILURE(&env, kCodeGetLocal0);
}


TEST_F(DecoderTest, GetLocal1_fail_no_locals) {
  EXPECT_FAILURE(&env_i_i, kCodeGetLocal1);
}


TEST_F(DecoderTest, GetLocal_off_end) {
  static const byte code[] = {kExprGetLocal};
  EXPECT_FAILURE(&env_i_i, code);
}


TEST_F(DecoderTest, GetLocal_varint) {
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


TEST_F(DecoderTest, Binops_off_end) {
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
TEST_F(DecoderTest, Nop) {
  static const byte code[] = {kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, SetLocal0_param) {
  static const byte code[] = {kExprSetLocal, 0, kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, SetLocal0_local) {
  byte code[] = {kExprSetLocal, 0, kExprInt8Const, 0};
  FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), 1);

  EXPECT_VERIFIES(&env, code);
}


TEST_F(DecoderTest, SetLocalN_local) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env = CreateInt32FunctionEnv(sigs.i_v(), i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprSetLocal, j, kExprInt8Const, i};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(DecoderTest, Switches0) {
  static const byte code[] = {kStmtSwitch, 0, kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 0, kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(DecoderTest, Switches1) {
  static const byte code[] = {kStmtSwitch, 1, kExprInt8Const, 0, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 1, kExprInt8Const, 0, kStmtBlock,
                                0};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(DecoderTest, Switches2) {
  static const byte code[] = {kStmtSwitch, 2, kExprInt8Const, 0, kStmtNop,
                              kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 2, kExprInt8Const, 0, kStmtBlock,
                                0, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(DecoderTest, Switches4) {
  static const byte code[] = {kStmtSwitch, 4, kExprInt8Const, 0, kStmtNop,
                              kStmtNop, kStmtNop, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, code);
  static const byte codenf[] = {kStmtSwitchNf, 4, kExprInt8Const, 0, kStmtBlock,
                                0, kStmtNop, kStmtNop, kStmtNop};
  EXPECT_VERIFIES(&env_v_v, codenf);
}


TEST_F(DecoderTest, Switches4_break) {
  for (int i = 0; i < 4; i++) {
    byte code[] = {kStmtSwitch, 4, kExprInt8Const, 0, kStmtBlock, 0, kStmtBlock,
                   0, kStmtBlock, 0, kStmtBlock, 0};
    code[4 + i * 2] = kStmtBreak;
    EXPECT_VERIFIES(&env_v_v, code);

    byte codenf[] = {kStmtSwitchNf, 4, kExprInt8Const, 0, kStmtBlock, 0,
                     kStmtBlock, 0, kStmtBlock, 0, kStmtBlock, 0};
    codenf[4 + i * 2] = kStmtBreak;
    EXPECT_VERIFIES(&env_v_v, codenf);
  }
}


TEST_F(DecoderTest, Block0) {
  static const byte code[] = {kStmtBlock, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Block0_fallthru1) {
  static const byte code[] = {kStmtBlock, 0, kStmtBlock, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Block1) {
  static const byte code[] = {kStmtBlock, 1, kExprSetLocal, 0, kExprInt8Const,
                              0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block0_fallthru2) {
  static const byte code[] = {kStmtBlock, 0, kExprSetLocal, 0, kExprInt8Const,
                              0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block2) {
  static const byte code[] = {kStmtBlock, 2,                         // --
                              kExprSetLocal, 0, kExprInt8Const, 0,   // --
                              kExprSetLocal, 0, kExprInt8Const, 0};  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block2_fallthru) {
  static const byte code[] = {kStmtBlock, 2,                        // --
                              kExprSetLocal, 0, kExprInt8Const, 0,  // --
                              kExprSetLocal, 0, kExprInt8Const, 0,  // --
                              kExprInt8Const, 11};                  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, BlockN) {
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


TEST_F(DecoderTest, BlockN_off_end) {
  for (byte i = 2; i < 10; i++) {
    byte code[] = {kStmtBlock, i, kStmtContinue, 0};
    EXPECT_FAILURE(&env_v_v, code);
  }
}


TEST_F(DecoderTest, Block1_break) {
  static const byte code[] = {kStmtBlock, 1, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Block2_break) {
  static const byte code[] = {kStmtBlock, 2, kStmtNop, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Block1_continue) {
  static const byte code[] = {kStmtBlock, 1, kStmtContinue, 0};
  EXPECT_FAILURE(&env_v_v, code);
}


TEST_F(DecoderTest, Block2_continue) {
  static const byte code[] = {kStmtBlock, 2, kStmtNop, kStmtContinue, 0};
  EXPECT_FAILURE(&env_v_v, code);
}


TEST_F(DecoderTest, IfEmpty) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtNop};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, IfSet) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kExprSetLocal, 0,
                              kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, IfBlock1) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtBlock, 1,
                              kExprSetLocal, 0, kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, IfBlock2) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0,             // --
                              kStmtBlock, 2,                         // --
                              kExprSetLocal, 0, kExprInt8Const, 0,   // --
                              kExprSetLocal, 0, kExprInt8Const, 0};  // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, IfThenEmpty) {
  static const byte code[] = {kStmtIfThen, kExprGetLocal, 0, kStmtNop,
                              kStmtNop};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, IfThenSet) {
  static const byte code[] = {kStmtIfThen, kExprGetLocal, 0,         // --
                              kExprSetLocal, 0, kExprInt8Const, 0,   // --
                              kExprSetLocal, 0, kExprInt8Const, 1};  // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, Loop0) {
  static const byte code[] = {kStmtLoop, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Loop1) {
  static const byte code[] = {kStmtLoop, 1, kExprSetLocal, 0, kExprInt8Const,
                              0};
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, Loop2) {
  static const byte code[] = {kStmtLoop, 2,                          // --
                              kExprSetLocal, 0, kExprInt8Const, 0,   // --
                              kExprSetLocal, 0, kExprInt8Const, 0};  // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, Loop1_continue) {
  static const byte code[] = {kStmtLoop, 1, kStmtContinue, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Loop1_break) {
  static const byte code[] = {kStmtLoop, 1, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_v_v, code);
}


TEST_F(DecoderTest, Loop2_continue) {
  static const byte code[] = {kStmtLoop, 2,                         // --
                              kExprSetLocal, 0, kExprInt8Const, 0,  // --
                              kStmtContinue, 0};                    // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, Loop2_break) {
  static const byte code[] = {kStmtLoop, 2,                         // --
                              kExprSetLocal, 0, kExprInt8Const, 0,  // --
                              kStmtBreak, 0};                       // --
  EXPECT_VERIFIES(&env_v_i, code);
}


TEST_F(DecoderTest, ReturnVoid) {
  static const byte code[] = {kStmtReturn};
  EXPECT_VERIFIES(&env_v_v, code);
  EXPECT_FAILURE(&env_i_i, code);
  EXPECT_FAILURE(&env_i_f, code);
}


TEST_F(DecoderTest, UnreachableCode1) {
  EXPECT_FAILURE_INLINE(&env_v_v, kStmtReturn, kStmtNop);
}


TEST_F(DecoderTest, UnreachableCode2) {
  EXPECT_FAILURE_INLINE(&env_i_i, kStmtReturn, kExprInt8Const, 0, kStmtNop);
}


TEST_F(DecoderTest, UnreachableCode3) {
  EXPECT_FAILURE_INLINE(&env_i_i, kStmtLoop, 0, kExprInt8Const, 0, kStmtNop);
}


TEST_F(DecoderTest, Unreachable4) {
  EXPECT_FAILURE_INLINE(
      &env_i_i,
      WASM_BLOCK(2, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(1)),
                              WASM_SET_LOCAL(0, WASM_INT8(99))),
                 /*unreachable*/ WASM_RETURN(WASM_GET_LOCAL(0))),
      WASM_INT8(43));
}


TEST_F(DecoderTest, Codeiness) {
  VERIFY(kStmtLoop, 2,                         // --
         kExprSetLocal, 0, kExprInt8Const, 0,  // --
         kStmtBreak, 0);                       // --
}


TEST_F(DecoderTest, Ternary1) {
  VERIFY(kExprTernary, kExprGetLocal, 0, kExprInt8Const, 0, kExprInt8Const, 1);
  VERIFY(kExprTernary, kExprGetLocal, 0, kExprGetLocal, 0, kExprGetLocal, 0);
  VERIFY(kExprTernary, kExprGetLocal, 0, kExprInt32Add, kExprGetLocal, 0,
         kExprGetLocal, 0, kExprInt8Const, 1);
}


TEST_F(DecoderTest, Comma1) {
  VERIFY(kExprComma, kExprInt8Const, 0, kExprInt8Const, 1);
  VERIFY(kExprComma, kExprGetLocal, 0, kExprGetLocal, 0);
  VERIFY(kExprComma, kExprInt32Add, kExprGetLocal, 0, kExprGetLocal, 0,
         kExprInt8Const, 1);
}


TEST_F(DecoderTest, Ternary_off_end) {
  static const byte kCode[] = {kExprTernary, kExprGetLocal, 0, kExprGetLocal, 0,
                               kExprGetLocal, 0};
  for (size_t len = 1; len < arraysize(kCode); len++) {
    Verify(kError, &env_i_i, kCode, kCode + len);
  }
}


TEST_F(DecoderTest, Ternary_type) {
  {
    // float|double ? 1 : 2
    static const byte kCode[] = {kExprTernary, kExprGetLocal, 0, kExprInt8Const,
                                 1, kExprInt8Const, 2};
    EXPECT_FAILURE(&env_i_f, kCode);
    EXPECT_FAILURE(&env_i_d, kCode);
  }
  {
    // 1 ? float|double : 2
    static const byte kCode[] = {kExprTernary, kExprInt8Const, 1, kExprGetLocal,
                                 0, kExprInt8Const, 2};
    EXPECT_FAILURE(&env_i_f, kCode);
    EXPECT_FAILURE(&env_i_d, kCode);
  }
  {
    // stmt ? 0 : 1
    static const byte kCode[] = {kExprTernary, kStmtNop, kExprInt8Const, 0,
                                 kExprInt8Const, 1};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
  {
    // 0 ? stmt : 1
    static const byte kCode[] = {kExprTernary, kExprInt8Const, 0, kStmtNop,
                                 kExprInt8Const, 1};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
  {
    // 0 ? 1 : stmt
    static const byte kCode[] = {kExprTernary, kExprInt8Const, 0,
                                 kExprInt8Const, 1, 0, kStmtBlock};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
}


TEST_F(DecoderTest, Int64Local_param) {
  EXPECT_VERIFIES(&env_l_l, kCodeGetLocal0);
}


TEST_F(DecoderTest, Int64Locals) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env;
    init_env(&env, sigs.l_v());
    env.AddLocals(kAstInt64, i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprGetLocal, j};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(DecoderTest, Int32Binops) {
  TestBinop(kExprInt32Add, sigs.i_ii());
  TestBinop(kExprInt32Sub, sigs.i_ii());
  TestBinop(kExprInt32Mul, sigs.i_ii());
  TestBinop(kExprInt32SDiv, sigs.i_ii());
  TestBinop(kExprInt32UDiv, sigs.i_ii());
  TestBinop(kExprInt32SRem, sigs.i_ii());
  TestBinop(kExprInt32URem, sigs.i_ii());
  TestBinop(kExprInt32And, sigs.i_ii());
  TestBinop(kExprInt32Ior, sigs.i_ii());
  TestBinop(kExprInt32Xor, sigs.i_ii());
  TestBinop(kExprInt32Shl, sigs.i_ii());
  TestBinop(kExprInt32Shr, sigs.i_ii());
  TestBinop(kExprInt32Sar, sigs.i_ii());
  TestBinop(kExprInt32Eq, sigs.i_ii());
  TestBinop(kExprInt32Slt, sigs.i_ii());
  TestBinop(kExprInt32Sle, sigs.i_ii());
  TestBinop(kExprInt32Ult, sigs.i_ii());
  TestBinop(kExprInt32Ule, sigs.i_ii());
}


TEST_F(DecoderTest, DoubleBinops) {
  TestBinop(kExprFloat64Add, sigs.d_dd());
  TestBinop(kExprFloat64Sub, sigs.d_dd());
  TestBinop(kExprFloat64Mul, sigs.d_dd());
  TestBinop(kExprFloat64Div, sigs.d_dd());

  TestBinop(kExprFloat64Eq, sigs.i_dd());
  TestBinop(kExprFloat64Lt, sigs.i_dd());
  TestBinop(kExprFloat64Le, sigs.i_dd());
}


TEST_F(DecoderTest, FloatBinops) {
  TestBinop(kExprFloat32Add, sigs.f_ff());
  TestBinop(kExprFloat32Sub, sigs.f_ff());
  TestBinop(kExprFloat32Mul, sigs.f_ff());
  TestBinop(kExprFloat32Div, sigs.f_ff());

  TestBinop(kExprFloat32Eq, sigs.i_ff());
  TestBinop(kExprFloat32Lt, sigs.i_ff());
  TestBinop(kExprFloat32Le, sigs.i_ff());
}


TEST_F(DecoderTest, TypeConversions) {
  TestUnop(kExprInt32SConvertFloat32, kAstInt32, kAstFloat32);
  TestUnop(kExprInt32SConvertFloat64, kAstInt32, kAstFloat64);
  TestUnop(kExprInt32UConvertFloat32, kAstInt32, kAstFloat32);
  TestUnop(kExprInt32UConvertFloat64, kAstInt32, kAstFloat64);
  TestUnop(kExprFloat64SConvertInt32, kAstFloat64, kAstInt32);
  TestUnop(kExprFloat64UConvertInt32, kAstFloat64, kAstInt32);
  TestUnop(kExprFloat64ConvertFloat32, kAstFloat64, kAstFloat32);
  TestUnop(kExprFloat32SConvertInt32, kAstFloat32, kAstInt32);
  TestUnop(kExprFloat32UConvertInt32, kAstFloat32, kAstInt32);
  TestUnop(kExprFloat32ConvertFloat64, kAstFloat32, kAstFloat64);
}


TEST_F(DecoderTest, MacrosStmt) {
  VERIFY(WASM_SET_LOCAL(0, WASM_INT32(87348)));
  VERIFY(WASM_STORE_MEM(kMemInt32, WASM_INT8(24), WASM_INT8(40)));
  VERIFY(WASM_IF(WASM_GET_LOCAL(0), WASM_NOP));
  VERIFY(WASM_IF_THEN(WASM_GET_LOCAL(0), WASM_NOP, WASM_NOP));
  VERIFY(WASM_NOP);
  VERIFY(WASM_BLOCK(1, WASM_NOP));
  VERIFY(WASM_LOOP(1, WASM_NOP));
  VERIFY(WASM_LOOP(1, WASM_BREAK(0)));
  VERIFY(WASM_LOOP(1, WASM_CONTINUE(0)));
}

TEST_F(DecoderTest, MacrosReturn) {
  EXPECT_VERIFIES_INLINE(&env_i_i, WASM_RETURN(WASM_ZERO));
  EXPECT_VERIFIES_INLINE(&env_l_l, WASM_RETURN(WASM_INT64(0)));
  EXPECT_VERIFIES_INLINE(&env_f_ff, WASM_RETURN(WASM_FLOAT32(0.0)));
  EXPECT_VERIFIES_INLINE(&env_d_dd, WASM_RETURN(WASM_FLOAT64(0.0)));

  static LocalType kIntTypes[] = {kAstInt32, kAstInt32};
  FunctionSig sig_ii_v(2, 0, kIntTypes);
  FunctionEnv env_ii_v;
  init_env(&env_ii_v, &sig_ii_v);

  EXPECT_VERIFIES_INLINE(&env_ii_v, WASM_RETURN(WASM_ZERO, WASM_ZERO));
}


TEST_F(DecoderTest, MacrosVariadic) {
  VERIFY(WASM_BLOCK(2, WASM_NOP, WASM_NOP));
  VERIFY(WASM_BLOCK(3, WASM_NOP, WASM_NOP, WASM_NOP));
  VERIFY(WASM_LOOP(2, WASM_NOP, WASM_NOP));
  VERIFY(WASM_LOOP(3, WASM_NOP, WASM_NOP, WASM_NOP));
}


TEST_F(DecoderTest, MacrosNestedBlocks) {
  VERIFY(WASM_BLOCK(2, WASM_NOP, WASM_BLOCK(2, WASM_NOP, WASM_NOP)));
  VERIFY(WASM_BLOCK(3, WASM_NOP,                          // --
                    WASM_BLOCK(2, WASM_NOP, WASM_NOP),    // --
                    WASM_BLOCK(2, WASM_NOP, WASM_NOP)));  // --
  VERIFY(WASM_BLOCK(1, WASM_BLOCK(1, WASM_BLOCK(2, WASM_NOP, WASM_NOP))));
}


TEST_F(DecoderTest, MultipleReturn) {
  static LocalType kIntTypes5[] = {kAstInt32, kAstInt32, kAstInt32, kAstInt32,
                                   kAstInt32};
  FunctionSig sig_ii_v(2, 0, kIntTypes5);
  FunctionEnv env_ii_v;
  init_env(&env_ii_v, &sig_ii_v);
  EXPECT_VERIFIES_INLINE(&env_ii_v, WASM_RETURN(WASM_ZERO, WASM_ONE));
  EXPECT_FAILURE_INLINE(&env_ii_v, WASM_RETURN(WASM_ZERO));

  FunctionSig sig_iii_v(3, 0, kIntTypes5);
  FunctionEnv env_iii_v;
  init_env(&env_iii_v, &sig_iii_v);
  EXPECT_VERIFIES_INLINE(&env_iii_v,
                         WASM_RETURN(WASM_ZERO, WASM_ONE, WASM_INT8(44)));
  EXPECT_FAILURE_INLINE(&env_iii_v, WASM_RETURN(WASM_ZERO, WASM_ONE));
}


TEST_F(DecoderTest, MultipleReturn_fallthru) {
  static LocalType kIntTypes5[] = {kAstInt32, kAstInt32, kAstInt32, kAstInt32,
                                   kAstInt32};
  FunctionSig sig_ii_v(2, 0, kIntTypes5);
  FunctionEnv env_ii_v;
  init_env(&env_ii_v, &sig_ii_v);

  EXPECT_VERIFIES_INLINE(&env_ii_v, WASM_ZERO, WASM_ONE);
  EXPECT_FAILURE_INLINE(&env_ii_v, WASM_ZERO);

  FunctionSig sig_iii_v(3, 0, kIntTypes5);
  FunctionEnv env_iii_v;
  init_env(&env_iii_v, &sig_iii_v);
  EXPECT_VERIFIES_INLINE(&env_iii_v, WASM_ZERO, WASM_ONE, WASM_INT8(44));
  EXPECT_FAILURE_INLINE(&env_iii_v, WASM_ZERO, WASM_ONE);
}


TEST_F(DecoderTest, MacrosInt32) {
  VERIFY(WASM_INT32_ADD(WASM_GET_LOCAL(0), WASM_INT8(12)));
  VERIFY(WASM_INT32_SUB(WASM_GET_LOCAL(0), WASM_INT8(13)));
  VERIFY(WASM_INT32_MUL(WASM_GET_LOCAL(0), WASM_INT8(14)));
  VERIFY(WASM_INT32_SDIV(WASM_GET_LOCAL(0), WASM_INT8(15)));
  VERIFY(WASM_INT32_UDIV(WASM_GET_LOCAL(0), WASM_INT8(16)));
  VERIFY(WASM_INT32_SREM(WASM_GET_LOCAL(0), WASM_INT8(17)));
  VERIFY(WASM_INT32_UREM(WASM_GET_LOCAL(0), WASM_INT8(18)));
  VERIFY(WASM_INT32_AND(WASM_GET_LOCAL(0), WASM_INT8(19)));
  VERIFY(WASM_INT32_IOR(WASM_GET_LOCAL(0), WASM_INT8(20)));
  VERIFY(WASM_INT32_XOR(WASM_GET_LOCAL(0), WASM_INT8(21)));
  VERIFY(WASM_INT32_SHL(WASM_GET_LOCAL(0), WASM_INT8(22)));
  VERIFY(WASM_INT32_SHR(WASM_GET_LOCAL(0), WASM_INT8(23)));
  VERIFY(WASM_INT32_SAR(WASM_GET_LOCAL(0), WASM_INT8(24)));
  VERIFY(WASM_INT32_EQ(WASM_GET_LOCAL(0), WASM_INT8(25)));
  VERIFY(WASM_INT32_SLT(WASM_GET_LOCAL(0), WASM_INT8(26)));
  VERIFY(WASM_INT32_SLE(WASM_GET_LOCAL(0), WASM_INT8(27)));
  VERIFY(WASM_INT32_ULT(WASM_GET_LOCAL(0), WASM_INT8(28)));
  VERIFY(WASM_INT32_ULE(WASM_GET_LOCAL(0), WASM_INT8(29)));
}


TEST_F(DecoderTest, AllSimpleExpressions) {
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


TEST_F(DecoderTest, AllLoadMemCombinations) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType local_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kMemTypes); j++) {
      MemType mem_type = kMemTypes[j];
      byte code[] = {
          kStmtReturn, WasmOpcodes::LoadStoreOpcodeOf(mem_type, false),
          WasmOpcodes::LoadStoreAccessOf(mem_type), kExprInt8Const, 0};
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


TEST_F(DecoderTest, AllStoreMemCombinations) {
  for (size_t i = 0; i < arraysize(kLocalTypes); i++) {
    LocalType local_type = kLocalTypes[i];
    for (size_t j = 0; j < arraysize(kMemTypes); j++) {
      MemType mem_type = kMemTypes[j];
      byte code[] = {WasmOpcodes::LoadStoreOpcodeOf(mem_type, true),
                     WasmOpcodes::LoadStoreAccessOf(mem_type), kExprInt8Const,
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


namespace {
// A helper for tests that require a module environment for functions and
// globals.
class TestModuleEnv : public ModuleEnv {
 public:
  TestModuleEnv() {
    mem_start = 0;
    mem_end = 0;
    module = &mod;
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

TEST_F(DecoderTest, SimpleCalls) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_v());
  module_env.AddFunction(sigs.i_i());
  module_env.AddFunction(sigs.i_ii());

  EXPECT_VERIFIES_INLINE(env, WASM_CALL_FUNCTION(0));
  EXPECT_VERIFIES_INLINE(env, WASM_CALL_FUNCTION(1, WASM_INT8(27)));
  EXPECT_VERIFIES_INLINE(env,
                         WASM_CALL_FUNCTION(2, WASM_INT8(37), WASM_INT8(77)));
}


TEST_F(DecoderTest, CallsWithTooFewArguments) {
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


TEST_F(DecoderTest, CallsWithSpilloverArgs) {
  static LocalType a_i_ff[] = {kAstInt32, kAstFloat32, kAstFloat32};
  FunctionSig sig_i_ff(1, 2, a_i_ff);
  FunctionEnv env_i_ff;
  init_env(&env_i_ff, &sig_i_ff);

  TestModuleEnv module_env;
  env_i_ff.module = &module_env;
  env_i_i.module = &module_env;
  env_f_ff.module = &module_env;

  module_env.AddFunction(&sig_i_ff);

  EXPECT_VERIFIES_INLINE(
      &env_i_i, WASM_CALL_FUNCTION(0, WASM_FLOAT32(0.1), WASM_FLOAT32(0.1)));

  EXPECT_VERIFIES_INLINE(
      &env_i_ff, WASM_CALL_FUNCTION(0, WASM_FLOAT32(0.1), WASM_FLOAT32(0.1)));

  EXPECT_FAILURE_INLINE(
      &env_f_ff, WASM_CALL_FUNCTION(0, WASM_FLOAT32(0.1), WASM_FLOAT32(0.1)));

  EXPECT_FAILURE_INLINE(
      &env_i_i, WASM_CALL_FUNCTION(0, WASM_FLOAT32(0.1), WASM_FLOAT32(0.1),
                                   WASM_FLOAT32(0.2)));

  EXPECT_VERIFIES_INLINE(
      &env_f_ff, WASM_CALL_FUNCTION(0, WASM_FLOAT32(0.1), WASM_FLOAT32(0.1),
                                    WASM_FLOAT32(11)));
}


TEST_F(DecoderTest, CallsWithMismatchedSigs2) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_i());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_INT64(17)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_FLOAT32(17.1)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_FLOAT64(17.1)));
}


TEST_F(DecoderTest, CallsWithMismatchedSigs3) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddFunction(sigs.i_f());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_INT8(17)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_INT64(27)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(0, WASM_FLOAT64(37.2)));

  module_env.AddFunction(sigs.i_d());

  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_INT8(16)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_INT64(16)));
  EXPECT_FAILURE_INLINE(env, WASM_CALL_FUNCTION(1, WASM_FLOAT32(17.6)));
}


TEST_F(DecoderTest, Int32Globals) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemInt8);
  module_env.AddGlobal(kMemUint8);
  module_env.AddGlobal(kMemInt16);
  module_env.AddGlobal(kMemUint16);
  module_env.AddGlobal(kMemInt32);
  module_env.AddGlobal(kMemUint32);

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


TEST_F(DecoderTest, Int32Globals_fail) {
  FunctionEnv* env = &env_i_i;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemInt64);
  module_env.AddGlobal(kMemUint64);
  module_env.AddGlobal(kMemFloat32);
  module_env.AddGlobal(kMemFloat64);

  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(1)));
  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(2)));
  EXPECT_FAILURE_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(3)));

  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(1, WASM_GET_LOCAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(2, WASM_GET_LOCAL(0)));
  EXPECT_FAILURE_INLINE(env, WASM_STORE_GLOBAL(3, WASM_GET_LOCAL(0)));
}


TEST_F(DecoderTest, Int64Globals) {
  FunctionEnv* env = &env_l_l;
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemInt64);
  module_env.AddGlobal(kMemUint64);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(1)));

  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(1, WASM_GET_LOCAL(0)));
}


TEST_F(DecoderTest, Float32Globals) {
  FunctionEnv env_f_ff;
  FunctionEnv* env = &env_f_ff;
  init_env(env, sigs.f_ff());
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemFloat32);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
}


TEST_F(DecoderTest, Float64Globals) {
  FunctionEnv env_d_dd;
  FunctionEnv* env = &env_d_dd;
  init_env(env, sigs.d_dd());
  TestModuleEnv module_env;
  env->module = &module_env;

  module_env.AddGlobal(kMemFloat64);

  EXPECT_VERIFIES_INLINE(env, WASM_RETURN(WASM_LOAD_GLOBAL(0)));
  EXPECT_VERIFIES_INLINE(env, WASM_STORE_GLOBAL(0, WASM_GET_LOCAL(0)));
}


TEST_F(DecoderTest, AllLoadGlobalCombinations) {
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


TEST_F(DecoderTest, AllStoreGlobalCombinations) {
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


TEST_F(DecoderTest, BreakNesting1) {
  for (int i = 0; i < 5; i++) {
    // (block[2] (loop[2] (if (get p) break[N]) (set p 1)) (return p))
    byte code[] = {
        WASM_BLOCK(2, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(i)),
                                WASM_SET_LOCAL(0, WASM_INT8(1))),
                   WASM_RETURN(WASM_GET_LOCAL(0)))};
    if (i == 0) {
      EXPECT_VERIFIES(&env_i_i, code);
    } else {
      EXPECT_FAILURE(&env_i_i, code);
    }
  }
}


TEST_F(DecoderTest, BreakNesting2) {
  env_v_v.AddLocals(kAstInt32, 1);
  for (int i = 0; i < 5; i++) {
    // (block[2] (loop[2] (if (get p) break[N]) (set p 1)) (return p)) (11)
    byte code[] = {
        WASM_BLOCK(1, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(i)),
                                WASM_SET_LOCAL(0, WASM_INT8(1)))),
        WASM_INT8(11)};
    if (i < 2) {
      EXPECT_VERIFIES(&env_v_v, code);
    } else {
      EXPECT_FAILURE(&env_v_v, code);
    }
  }
}


TEST_F(DecoderTest, BreakNesting3) {
  env_v_v.AddLocals(kAstInt32, 1);
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


TEST_F(DecoderTest, BreakNesting_6_levels) {
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


TEST_F(DecoderTest, ContinueNesting_6_levels) {
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

//--------------------------------------------------------------------------
// TODO(titzer): not a real test.
//--------------------------------------------------------------------------
void TestWasmDecodingSpeed();


TEST_F(DecoderTest, Speed) {
  //  TestWasmDecodingSpeed();
}
}
}
}
