// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/js-graph.h"
#include "src/compiler/graph-visualizer.h"

#include "src/wasm/wasm-opcodes.h"
#include "src/wasm/decoder.h"

#include "test/cctest/cctest.h"
#include "test/cctest/compiler/graph-builder-tester.h"
#include "test/cctest/compiler/value-helper.h"

#if V8_TURBOFAN_TARGET

using namespace v8::base;
using namespace v8::internal;
using namespace v8::internal::compiler;
using namespace v8::internal::wasm;

#define LE32(x)                                                                \
  static_cast<byte>(x), static_cast<byte>(x >> 8), static_cast<byte>(x >> 16), \
      static_cast<byte>(x >> 24)


// Helpers for many common signatures that involve int32 types.
static AstType kIntTypes5[] = {kAstInt32, kAstInt32, kAstInt32, kAstInt32,
                               kAstInt32};

struct CommonSignatures {
  CommonSignatures()
      : sig_i_v(1, 0, kIntTypes5),
        sig_i_i(1, 1, kIntTypes5),
        sig_i_ii(1, 2, kIntTypes5),
        sig_i_iii(1, 3, kIntTypes5) {
    init_env(&env_i_v, &sig_i_v);
    init_env(&env_i_i, &sig_i_i);
    init_env(&env_i_ii, &sig_i_ii);
    init_env(&env_i_iii, &sig_i_iii);
  }

  FunctionSig sig_i_v;
  FunctionSig sig_i_i;
  FunctionSig sig_i_ii;
  FunctionSig sig_i_iii;
  FunctionEnv env_i_v;
  FunctionEnv env_i_i;
  FunctionEnv env_i_ii;
  FunctionEnv env_i_iii;

  void init_env(FunctionEnv* env, FunctionSig* sig) {
    env->module = nullptr;
    env->sig = sig;
    env->local_int32_count = 0;
    env->local_float64_count = 0;
    env->local_float32_count = 0;
    env->total_locals = 0;
  }
};


// A helper class to build graphs from Wasm bytecode, generate machine
// code, and run that code.
template <typename ReturnType>
class WasmRunner : public GraphBuilderTester<ReturnType> {
 public:
  WasmRunner(MachineType p0 = kMachNone, MachineType p1 = kMachNone,
               MachineType p2 = kMachNone, MachineType p3 = kMachNone,
               MachineType p4 = kMachNone)
      : GraphBuilderTester<ReturnType>(p0, p1, p2, p3, p4),
        jsgraph(this->isolate(), this->graph(), this->common(), nullptr,
                this->machine()) {
    if (p1 != kMachNone) {
      function_env = &sigs_.env_i_ii;
    } else if (p0 != kMachNone) {
      function_env = &sigs_.env_i_i;
    } else {
      function_env = &sigs_.env_i_v;
    }
  }

  JSGraph jsgraph;
  CommonSignatures sigs_;
  FunctionEnv* function_env;

  void Build(const byte* start, const byte* end) {
    Result result = BuildTFGraph(&jsgraph, function_env, start, end);
    if (result.error_msg != nullptr) {
      ptrdiff_t pc = result.error_pc - result.pc;
      ptrdiff_t pt = result.error_pt - result.pc;
      std::ostringstream str;
      str << "Verification failed: " << result.error_code << " pc = +" << pc
          << ", pt = +" << pt << ", msg = " << result.error_msg;
      FATAL(str.str().c_str());
    }
    if (FLAG_trace_turbo_graph) {
      OFStream os(stdout);
      os << AsRPO(*jsgraph.graph());
    }
  }
};


TEST(Run_WasmInt8Const) {
  WasmRunner<int8_t> r;
  const byte kExpectedValue = 121;
  static const byte kCode[] = {kStmtReturn, 1, kExprInt8Const, kExpectedValue};

  r.Build(kCode, kCode + arraysize(kCode));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt8Const_all) {
  for (int value = -128; value <= 127; value++) {
    WasmRunner<int8_t> r;
    byte kCode[] = {kStmtReturn, 1, kExprInt8Const, static_cast<byte>(value)};

    r.Build(kCode, kCode + arraysize(kCode));
    CHECK_EQ(value, r.Call());
  }
}


TEST(Run_WasmInt32Const) {
  WasmRunner<int32_t> r;
  const int32_t kExpectedValue = 0x11223344;
  static const byte kCode[] = {kStmtReturn, 1, kExprInt32Const,
                               LE32(kExpectedValue)};

  r.Build(kCode, kCode + arraysize(kCode));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt32Const_many) {
  FOR_INT32_INPUTS(i) {
    WasmRunner<int32_t> r;
    const int32_t kExpectedValue = *i;
    byte kCode[] = {kStmtReturn, 1, kExprInt32Const, LE32(kExpectedValue)};

    r.Build(kCode, kCode + arraysize(kCode));
    CHECK_EQ(kExpectedValue, r.Call());
  }
}


TEST(Run_WasmInt32Param0) {
  WasmRunner<int32_t> r(kMachInt32);
  static const byte kCode[] = {kStmtReturn, 1, kExprGetLocal, 0};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(*i)); }
}


TEST(Run_WasmInt32Param1) {
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  static const byte kCode[] = {kStmtReturn, 1, kExprGetLocal, 1};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(-111, *i)); }
}


TEST(Run_WasmInt32Add) {
  WasmRunner<int32_t> r;
  // return 11 + 44
  static const byte kCode[] = {kStmtReturn, 1, kExprInt32Add, kExprInt8Const,
                               11, kExprInt8Const, 44};

  r.Build(kCode, kCode + arraysize(kCode));
  CHECK_EQ(55, r.Call());
}


TEST(Run_WasmInt32Add_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // return p0 + 13
  static const byte kCode[] = {kStmtReturn, 1, kExprInt32Add, kExprInt8Const,
                               13, kExprGetLocal, 0};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i + 13, r.Call(*i)); }
}


TEST(Run_WasmInt32Add_P2) {
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  // return p0 + p1
  static const byte kCode[] = {kStmtReturn, 1, kExprInt32Add, kExprGetLocal, 0,
                               kExprGetLocal, 1};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) {
    FOR_INT32_INPUTS(j) {
      int32_t expected = static_cast<int32_t>(static_cast<uint32_t>(*i) +
                                              static_cast<uint32_t>(*j));
      CHECK_EQ(expected, r.Call(*i, *j));
    }
  }
}

// TODO: test all Int32 binops

TEST(Run_Wasm_IfThen_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // if (p0) return 11; else return 22;
  static const byte kCode[] = {kStmtIfThen, kExprGetLocal, 0, kStmtReturn, 1,
                               kExprInt8Const, 11, kStmtReturn, 1,
                               kExprInt8Const, 22};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 11 : 22;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Block_If_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // { if (p0) return 51; return 52; }
  static const byte kCode[] = {kStmtBlock, 2, kStmtIf, kExprGetLocal, 0,
                               kStmtReturn, 1, kExprInt8Const, 51, kStmtReturn,
                               1, kExprInt8Const, 52};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 51 : 52;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Block_IfThen_P_assign) {
  WasmRunner<int32_t> r(kMachInt32);
  // { if (p0) p0 = 71; else p0 = 72; return p0; }
  static const byte kCode[] = {kStmtBlock, 2, kStmtIfThen, kExprGetLocal, 0,
                               kStmtSetLocal, 0, kExprInt8Const, 71,
                               kStmtSetLocal, 0, kExprInt8Const, 72,
                               kStmtReturn, 1, kExprGetLocal, 0};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 71 : 72;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Block_If_P_assign) {
  WasmRunner<int32_t> r(kMachInt32);
  // { if (p0) p0 = 61; return p0; }
  static const byte kCode[] = {kStmtBlock, 2, kStmtIf, kExprGetLocal, 0,
                               kStmtSetLocal, 0, kExprInt8Const, 61,
                               kStmtReturn, 1, kExprGetLocal, 0};

  r.Build(kCode, kCode + arraysize(kCode));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 61 : *i;
    CHECK_EQ(expected, r.Call(*i));
  }
}


#endif  // V8_TURBOFAN_TARGET
