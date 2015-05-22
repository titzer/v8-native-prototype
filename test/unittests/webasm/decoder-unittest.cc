// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "test/unittests/test-utils.h"
#include "src/webasm/decoder.h"

namespace v8 {
namespace internal {
namespace webasm {

static AstType kIntTypes5[] = {kAstInt32, kAstInt32, kAstInt32, kAstInt32,
                               kAstInt32};

static AstType kIntFloatTypes5[] = {kAstInt32, kAstFloat32, kAstFloat32,
                                    kAstFloat32, kAstFloat32};

static AstType kIntDoubleTypes5[] = {kAstInt32, kAstFloat64, kAstFloat64,
                                     kAstFloat64, kAstFloat64};

static const byte kCodeGetLocal0[] = {kExprGetLocal, 0};
static const byte kCodeGetLocal1[] = {kExprGetLocal, 1};
static const byte kCodeSetLocal0[] = {kStmtSetLocal, 0, kExprInt8Const, 0};

static const WebAsmOpcode kInt32BinopOpcodes[] = {
    kExprInt32Add,  kExprInt32Sub,  kExprInt32Mul,  kExprInt32SDiv,
    kExprInt32UDiv, kExprInt32SMod, kExprInt32UMod, kExprInt32And,
    kExprInt32Ior,  kExprInt32Xor,  kExprInt32Shl,  kExprInt32Shr,
    kExprInt32Sar,  kExprInt32Eq,   kExprInt32Slt,  kExprInt32Sle,
    kExprInt32Ult,  kExprInt32Ule};


class DecoderTest : public TestWithZone {
 public:
  DecoderTest()
      : TestWithZone(),
        sig_i_v(1, 0, kIntTypes5),
        sig_i_i(1, 1, kIntTypes5),
        sig_i_ii(1, 2, kIntTypes5),
        sig_i_iii(1, 3, kIntTypes5),
        sig_i_iiii(1, 4, kIntTypes5),
        sig_i_f(1, 1, kIntFloatTypes5),
        sig_i_d(1, 1, kIntDoubleTypes5),
        sig_v_v(0, 0, kIntTypes5),
        sig_v_i(0, 1, kIntTypes5),
        sig_v_ii(0, 2, kIntTypes5),
        sig_v_iii(0, 3, kIntTypes5),
        sig_v_iiii(0, 4, kIntTypes5) {
    init_env(&env_i_i, &sig_i_i);
    init_env(&env_v_v, &sig_v_v);
    init_env(&env_i_f, &sig_i_f);
    init_env(&env_i_d, &sig_i_d);
  }

  FunctionSig sig_i_v;
  FunctionSig sig_i_i;
  FunctionSig sig_i_ii;
  FunctionSig sig_i_iii;
  FunctionSig sig_i_iiii;
  FunctionSig sig_i_f;
  FunctionSig sig_i_d;

  FunctionSig sig_v_v;
  FunctionSig sig_v_i;
  FunctionSig sig_v_ii;
  FunctionSig sig_v_iii;
  FunctionSig sig_v_iiii;

  FunctionEnv env_i_i;
  FunctionEnv env_v_v;
  FunctionEnv env_i_f;
  FunctionEnv env_i_d;

  void init_env(FunctionEnv* env, FunctionSig* sig) {
    env->module = nullptr;
    env->sig = sig;
    env->local_int32_count = 0;
    env->local_float64_count = 0;
    env->local_float32_count = 0;
    env->total_locals = sig->parameter_count();
  }
};


#define EXPECT_VERIFIES(env, x) Verify(kSuccess, env, x, x + arraysize(x))

#define EXPECT_FAILURE(env, x) Verify(kError, env, x, x + arraysize(x))

static FunctionEnv CreateInt32FunctionEnv(FunctionSig* sig, int count) {
  FunctionEnv env;
  env.module = nullptr;
  env.sig = sig;
  env.local_int32_count = count;
  env.local_float64_count = count;
  env.local_float32_count = count;
  env.total_locals = count + sig->parameter_count();
  return env;
}


// A wrapper around VerifyWebAsmCode() that renders a nice failure message.
static void Verify(ErrorCode expected, FunctionEnv* env, const byte* start,
                   const byte* end) {
  Result result = VerifyWebAsmCode(env, start, end);
  if (result.error_code != expected) {
    ptrdiff_t pc = result.error_pc - result.pc;
    ptrdiff_t pt = result.error_pt - result.pc;
    std::ostringstream str;
    if (expected == kSuccess) {
      str << "Verification failed: " << result.error_code << " pc = +" << pc
          << ", pt = +" << pt;
    } else {
      str << "Verification expected: " << expected << ", but got "
          << result.error_code;
      if (result.error_code != kSuccess) {
        str << " pc = +" << pc << ", pt = +" << pt;
      }
    }
    FATAL(str.str().c_str());
  }
}


TEST_F(DecoderTest, Int8Const) {
  byte code[] = {kExprInt8Const, 0};
  for (int i = -128; i < 128; i++) {
    code[1] = static_cast<byte>(i);
    EXPECT_VERIFIES(&env_i_i, code);
  }
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
  EXPECT_VERIFIES(&env_i_i, kCodeGetLocal0);
}


TEST_F(DecoderTest, GetLocal0_param_n) {
  FunctionSig* sigs[] = {&sig_i_i, &sig_i_ii, &sig_i_iii, &sig_i_iiii};

  for (size_t i = 0; i < arraysize(sigs); i++) {
    FunctionEnv env = CreateInt32FunctionEnv(sigs[i], 0);
    EXPECT_VERIFIES(&env, kCodeGetLocal0);
  }
}


TEST_F(DecoderTest, GetLocalN_local) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env = CreateInt32FunctionEnv(&sig_i_v, i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kExprGetLocal, j};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(DecoderTest, GetLocal0_fail_no_params) {
  FunctionEnv env = CreateInt32FunctionEnv(&sig_i_v, 0);

  EXPECT_FAILURE(&env, kCodeGetLocal0);
}


TEST_F(DecoderTest, GetLocal1_fail_no_locals) {
  EXPECT_FAILURE(&env_i_i, kCodeGetLocal1);
}


TEST_F(DecoderTest, GetLocal_off_end) {
  static const byte code[] = {kExprGetLocal};
  EXPECT_FAILURE(&env_i_i, code);
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
TEST_F(DecoderTest, SetLocal0_param) {
  static const byte code[] = {kStmtSetLocal, 0, kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, SetLocal0_local) {
  byte code[] = {kStmtSetLocal, 0, kExprInt8Const, 0};
  FunctionEnv env = CreateInt32FunctionEnv(&sig_i_v, 1);

  EXPECT_VERIFIES(&env, code);
}


TEST_F(DecoderTest, SetLocalN_local) {
  for (byte i = 1; i < 8; i++) {
    FunctionEnv env = CreateInt32FunctionEnv(&sig_i_v, i);
    for (byte j = 0; j < i; j++) {
      byte code[] = {kStmtSetLocal, j, kExprInt8Const, i};
      EXPECT_VERIFIES(&env, code);
    }
  }
}


TEST_F(DecoderTest, Block0) {
  static const byte code[] = {kStmtBlock, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block1) {
  static const byte code[] = {kStmtBlock, 1, kStmtSetLocal, 0, kExprInt8Const,
                              0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block2) {
  static const byte code[] = {kStmtBlock, 2,                         // --
                              kStmtSetLocal, 0, kExprInt8Const, 0,   // --
                              kStmtSetLocal, 0, kExprInt8Const, 0};  // --
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
    Verify(kSuccess, &env_i_i, code, code + total);
    free(code);
  }
}


TEST_F(DecoderTest, BlockN_off_end) {
  for (byte i = 2; i < 10; i++) {
    byte code[] = {kStmtBlock, i, kStmtContinue, 0};
    EXPECT_FAILURE(&env_i_i, code);
  }
}


TEST_F(DecoderTest, Block1_break) {
  static const byte code[] = {kStmtBlock, 1, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block2_break) {
  static const byte code[] = {kStmtBlock, 2, kStmtBlock, 0, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Block1_continue) {
  static const byte code[] = {kStmtBlock, 1, kStmtContinue, 0};
  EXPECT_FAILURE(&env_i_i, code);
}


TEST_F(DecoderTest, Block2_continue) {
  static const byte code[] = {kStmtBlock, 2, kStmtBlock, 0, kStmtContinue, 0};
  EXPECT_FAILURE(&env_i_i, code);
}


TEST_F(DecoderTest, IfEmpty) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtBlock, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, IfSet) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtSetLocal, 0,
                              kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, IfBlock1) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0, kStmtBlock, 1,
                              kStmtSetLocal, 0, kExprInt8Const, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, IfBlock2) {
  static const byte code[] = {kStmtIf, kExprGetLocal, 0,             // --
                              kStmtBlock, 2,                         // --
                              kStmtSetLocal, 0, kExprInt8Const, 0,   // --
                              kStmtSetLocal, 0, kExprInt8Const, 0};  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, IfThenEmpty) {
  static const byte code[] = {kStmtIfThen, kExprGetLocal, 0, kStmtBlock, 0,
                              kStmtBlock, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, IfThenSet) {
  static const byte code[] = {kStmtIfThen, kExprGetLocal, 0,         // --
                              kStmtSetLocal, 0, kExprInt8Const, 0,   // --
                              kStmtSetLocal, 0, kExprInt8Const, 1};  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop0) {
  static const byte code[] = {kStmtLoop, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop1) {
  static const byte code[] = {kStmtLoop, 1, kStmtSetLocal, 0, kExprInt8Const,
                              0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop2) {
  static const byte code[] = {kStmtLoop, 2,                          // --
                              kStmtSetLocal, 0, kExprInt8Const, 0,   // --
                              kStmtSetLocal, 0, kExprInt8Const, 0};  // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop1_continue) {
  static const byte code[] = {kStmtLoop, 1, kStmtContinue, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop1_break) {
  static const byte code[] = {kStmtLoop, 1, kStmtBreak, 0};
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop2_continue) {
  static const byte code[] = {kStmtLoop, 2,                         // --
                              kStmtSetLocal, 0, kExprInt8Const, 0,  // --
                              kStmtContinue, 0};                    // --
  EXPECT_VERIFIES(&env_i_i, code);
}


TEST_F(DecoderTest, Loop2_break) {
  static const byte code[] = {kStmtLoop, 2,                         // --
                              kStmtSetLocal, 0, kExprInt8Const, 0,  // --
                              kStmtBreak, 0};                       // --
  EXPECT_VERIFIES(&env_i_i, code);
}


#define VERIFY(...)                                        \
  do {                                                     \
    static const byte code[] = {__VA_ARGS__};              \
    Verify(kSuccess, &env_i_i, code, code + sizeof(code)); \
  } while (false)


TEST_F(DecoderTest, Codeiness) {
  VERIFY(kStmtLoop, 2,                         // --
         kStmtSetLocal, 0, kExprInt8Const, 0,  // --
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
    static const byte kCode[] = {kExprTernary, kStmtBlock, 0, kExprInt8Const, 0,
                                 kExprInt8Const, 1};
    EXPECT_FAILURE(&env_i_i, kCode);
  }
  {
    // 0 ? stmt : 1
    static const byte kCode[] = {kExprTernary, kExprInt8Const, 0, kStmtBlock, 0,
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


//--------------------------------------------------------------------------
// TODO: not a real test.
//--------------------------------------------------------------------------
void TestWebAsmDecodingSpeed();


TEST_F(DecoderTest, Speed) {
  //  TestWebAsmDecodingSpeed();
}
}
}
}
