// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/js-graph.h"
#include "src/compiler/graph-visualizer.h"

#include "src/webasm/webasm-opcodes.h"
#include "src/webasm/webasm-macro-gen.h"
#include "src/webasm/decoder.h"

#include "test/cctest/cctest.h"
#include "test/cctest/compiler/graph-builder-tester.h"
#include "test/cctest/compiler/value-helper.h"

#if V8_TURBOFAN_TARGET

using namespace v8::base;
using namespace v8::internal;
using namespace v8::internal::compiler;
using namespace v8::internal::webasm;

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
    env->total_locals = sig->parameter_count();
  }
};


// A helper class to build graphs from WebAsm bytecode, generate machine
// code, and run that code.
template <typename ReturnType>
class WebAsmRunner : public GraphBuilderTester<ReturnType> {
 public:
  WebAsmRunner(MachineType p0 = kMachNone, MachineType p1 = kMachNone,
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


#define BUILD(r, ...)                      \
  do {                                     \
    byte code[] = {__VA_ARGS__};           \
    r.Build(code, code + arraysize(code)); \
  } while (false)


TEST(Run_WebAsmInt8Const) {
  WebAsmRunner<int8_t> r;
  const byte kExpectedValue = 121;
  // return(kExpectedValue)
  BUILD(r, WASM_RETURN(1, WASM_INT8(kExpectedValue)));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WebAsmInt8Const_all) {
  for (int value = -128; value <= 127; value++) {
    WebAsmRunner<int8_t> r;
    // return(value)
    BUILD(r, WASM_RETURN(1, WASM_INT8(value)));
    int8_t result = r.Call();
    CHECK_EQ(value, result);
  }
}


TEST(Run_WebAsmInt32Const) {
  WebAsmRunner<int32_t> r;
  const int32_t kExpectedValue = 0x11223344;
  // return(kExpectedValue)
  BUILD(r, WASM_RETURN(1, WASM_INT32(kExpectedValue)));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WebAsmInt32Const_many) {
  FOR_INT32_INPUTS(i) {
    WebAsmRunner<int32_t> r;
    const int32_t kExpectedValue = *i;
    // return(kExpectedValue)
    BUILD(r, WASM_RETURN(1, WASM_INT32(kExpectedValue)));
    CHECK_EQ(kExpectedValue, r.Call());
  }
}


TEST(Run_WebAsmInt32Param0) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // return(local[0])
  BUILD(r, WASM_RETURN(1, WASM_GET_LOCAL(0)));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(*i)); }
}


TEST(Run_WebAsmInt32Param1) {
  WebAsmRunner<int32_t> r(kMachInt32, kMachInt32);
  // return(local[1])
  BUILD(r, WASM_RETURN(1, WASM_GET_LOCAL(1)));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(-111, *i)); }
}


TEST(Run_WebAsmInt32Add) {
  WebAsmRunner<int32_t> r;
  // return 11 + 44
  BUILD(r, WASM_RETURN(1, WASM_INT32_ADD(WASM_INT8(11), WASM_INT8(44))));
  CHECK_EQ(55, r.Call());
}


TEST(Run_WebAsmInt32Add_P) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // return p0 + 13
  BUILD(r, WASM_RETURN(1, WASM_INT32_ADD(WASM_INT8(13), WASM_GET_LOCAL(0))));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i + 13, r.Call(*i)); }
}


TEST(Run_WebAsmInt32Add_P2) {
  WebAsmRunner<int32_t> r(kMachInt32, kMachInt32);
  // return p0 + p1
  BUILD(r,
        WASM_RETURN(1, WASM_INT32_ADD(WASM_GET_LOCAL(0), WASM_GET_LOCAL(1))));
  FOR_INT32_INPUTS(i) {
    FOR_INT32_INPUTS(j) {
      int32_t expected = static_cast<int32_t>(static_cast<uint32_t>(*i) +
                                              static_cast<uint32_t>(*j));
      CHECK_EQ(expected, r.Call(*i, *j));
    }
  }
}

// TODO: test all Int32 binops

TEST(Run_WebAsm_IfThen_P) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // if (p0) return 11; else return 22;
  BUILD(r, WASM_IF_THEN(WASM_GET_LOCAL(0),                // --
                        WASM_RETURN(1, WASM_INT8(11)),    // --
                        WASM_RETURN(1, WASM_INT8(22))));  // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 11 : 22;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_WebAsm_Block_If_P) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // { if (p0) return 51; return 52; }
  BUILD(r, WASM_BLOCK(2,                                       // --
                      WASM_IF(WASM_GET_LOCAL(0),               // --
                              WASM_RETURN(1, WASM_INT8(51))),  // --
                      WASM_RETURN(1, WASM_INT8(52))));         // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 51 : 52;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_WebAsm_Block_IfThen_P_assign) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // { if (p0) p0 = 71; else p0 = 72; return p0; }
  BUILD(r, WASM_BLOCK(2,                                               // --
                      WASM_IF_THEN(WASM_GET_LOCAL(0),                  // --
                                   WASM_SET_LOCAL(0, WASM_INT8(71)),   // --
                                   WASM_SET_LOCAL(0, WASM_INT8(72))),  // --
                      WASM_RETURN(1, WASM_GET_LOCAL(0))));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 71 : 72;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_WebAsm_Block_If_P_assign) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // { if (p0) p0 = 61; return p0; }
  BUILD(r, WASM_BLOCK(
               2, WASM_IF(WASM_GET_LOCAL(0), WASM_SET_LOCAL(0, WASM_INT8(61))),
               WASM_RETURN(1, WASM_GET_LOCAL(0))));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 61 : *i;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_WebAsm_Ternary_P) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // return p0 ? 11 : 22;
  BUILD(r, WASM_RETURN(1, WASM_TERNARY(WASM_GET_LOCAL(0),  // --
                                       WASM_INT8(11),      // --
                                       WASM_INT8(22))));   // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 11 : 22;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_WebAsm_Comma_P) {
  WebAsmRunner<int32_t> r(kMachInt32);
  // return p0, 17;
  BUILD(r, WASM_RETURN(1, WASM_COMMA(WASM_GET_LOCAL(0), WASM_INT8(17))));
  FOR_INT32_INPUTS(i) { CHECK_EQ(17, r.Call(*i)); }
}


#endif  // V8_TURBOFAN_TARGET
