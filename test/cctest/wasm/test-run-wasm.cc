// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include <stdlib.h>
#include <string.h>

#include "src/compiler/graph-visualizer.h"
#include "src/compiler/js-graph.h"

#include "src/wasm/decoder.h"
#include "src/wasm/wasm-macro-gen.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"

#include "test/cctest/cctest.h"
#include "test/cctest/compiler/graph-builder-tester.h"
#include "test/cctest/compiler/value-helper.h"

#include "test/cctest/wasm/test-signatures.h"

// TODO(titzer): pull WASM_64 up to a common header.
#if !V8_TARGET_ARCH_32_BIT || V8_TARGET_ARCH_X64
#define WASM_64 1
#else
#define WASM_64 0
#endif

using namespace v8::base;
using namespace v8::internal;
using namespace v8::internal::compiler;
using namespace v8::internal::wasm;

static void init_env(FunctionEnv* env, FunctionSig* sig) {
  env->module = nullptr;
  env->sig = sig;
  env->local_int32_count = 0;
  env->local_int64_count = 0;
  env->local_float32_count = 0;
  env->local_float64_count = 0;
  env->SumLocals();
}

const int kMaxGlobalsSize = 128;

// A helper for module environments that adds the ability to allocate memory
// and global variables.
class TestingModule : public ModuleEnv {
 public:
  TestingModule() : mem_size(0), global_offset(0) {
    globals_area = 0;
    mem_start = 0;
    mem_end = 0;
    module = nullptr;
    linker = nullptr;
    function_code = nullptr;
  }

  ~TestingModule() {
    if (mem_start) {
      free(raw_mem_start<byte>());
    }
    if (module) {
      if (module->globals) delete module->globals;
      if (module->functions) delete module->functions;
      if (globals_area) free(reinterpret_cast<byte*>(globals_area));
      delete module;
    }
  }

  byte* AddMemory(size_t size) {
    CHECK_EQ(0, mem_start);
    CHECK_EQ(0, mem_size);
    mem_start = reinterpret_cast<uintptr_t>(malloc(size));
    CHECK(mem_start);
    memset(raw_mem_start<byte>(), 0, size);
    mem_end = mem_start + size;
    mem_size = size;
    return raw_mem_start<byte>();
  }

  template <typename T>
  T* AddMemoryElems(size_t count) {
    AddMemory(count * sizeof(T));
    return raw_mem_start<T>();
  }

  template <typename T>
  T* AddGlobal(MemType mem_type) {
    WasmGlobal* global = AddGlobal(mem_type);
    return reinterpret_cast<T*>(globals_area + global->offset);
  }


  template <typename T>
  T* raw_mem_start() {
    DCHECK(mem_start);
    return reinterpret_cast<T*>(mem_start);
  }

  template <typename T>
  T* raw_mem_end() {
    DCHECK(mem_start);
    return reinterpret_cast<T*>(mem_end);
  }

  template <typename T>
  T raw_mem_at(int i) {
    DCHECK(mem_start);
    return reinterpret_cast<T*>(mem_start)[i];
  }

  // Zero-initialize the memory.
  void ZeroMemory() { memset(raw_mem_start<byte>(), 0, mem_size); }

  // Pseudo-randomly intialize the memory.
  void RandomizeMemory(unsigned seed = 88) {
    byte* raw = raw_mem_start<byte>();
    byte* end = raw_mem_end<byte>();
    while (raw < end) {
      *raw = static_cast<byte>(rand_r(&seed));
      raw++;
    }
  }

  WasmFunction* AddFunction(FunctionSig* sig, Handle<Code> code) {
    AllocModule();
    if (module->functions == nullptr) {
      module->functions = new std::vector<WasmFunction>();
      function_code = new std::vector<Handle<Code>>();
    }
    module->functions->push_back({sig, 0, 0, 0, 0, 0, 0, 0, false, false});
    function_code->push_back(code);
    return &module->functions->back();
  }

 private:
  size_t mem_size;
  unsigned global_offset;

  WasmGlobal* AddGlobal(MemType mem_type) {
    AllocModule();
    if (globals_area == 0) {
      globals_area = reinterpret_cast<uintptr_t>(malloc(kMaxGlobalsSize));
      module->globals = new std::vector<WasmGlobal>();
    }
    byte size = WasmOpcodes::MemSize(mem_type);
    global_offset = (global_offset + size - 1) & ~(size - 1);  // align
    module->globals->push_back({0, mem_type, global_offset, false});
    global_offset += size;
    CHECK_LT(global_offset, kMaxGlobalsSize);  // limit number of globals.
    return &module->globals->back();
  }
  void AllocModule() {
    if (module == nullptr) {
      module = new WasmModule();
      module->globals = nullptr;
      module->functions = nullptr;
      module->data_segments = nullptr;
    }
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
    init_env(&env_i_v, sigs.i_v());
    init_env(&env_i_i, sigs.i_i());
    init_env(&env_i_ii, sigs.i_ii());
    init_env(&env_v_v, sigs.v_v());
    init_env(&env_l_ll, sigs.l_ll());

    if (p1 != kMachNone) {
      function_env = &env_i_ii;
    } else if (p0 != kMachNone) {
      function_env = &env_i_i;
    } else {
      function_env = &env_i_v;
    }
  }

  JSGraph jsgraph;
  TestSignatures sigs;
  FunctionEnv env_i_v;
  FunctionEnv env_i_i;
  FunctionEnv env_i_ii;
  FunctionEnv env_v_v;
  FunctionEnv env_l_ll;
  FunctionEnv* function_env;

  void Build(const byte* start, const byte* end) {
    TreeResult result = BuildTFGraph(&jsgraph, function_env, start, end);
    if (result.failed()) {
      ptrdiff_t pc = result.error_pc - result.start;
      ptrdiff_t pt = result.error_pt - result.start;
      std::ostringstream str;
      str << "Verification failed: " << result.error_code << " pc = +" << pc;
      if (result.error_pt) str << ", pt = +" << pt;
      str << ", msg = " << result.error_msg.get();
      FATAL(str.str().c_str());
    }
    if (FLAG_trace_turbo_graph) {
      OFStream os(stdout);
      os << AsRPO(*jsgraph.graph());
    }
  }

  byte AllocateLocal(LocalType type) {
    int result = static_cast<int>(function_env->total_locals);
    function_env->AddLocals(type, 1);
    byte b = static_cast<byte>(result);
    CHECK_EQ(result, b);
    return b;
  }
};


// A helper for compiling functions that are only internally callable WASM code.
class WasmFunctionCompiler : public HandleAndZoneScope,
                             private GraphAndBuilders {
 public:
  explicit WasmFunctionCompiler(FunctionSig* sig)
      : GraphAndBuilders(main_zone()),
        jsgraph(this->isolate(), this->graph(), this->common(), nullptr,
                this->machine()) {
    init_env(&env, sig);
  }

  JSGraph jsgraph;
  FunctionEnv env;

  Isolate* isolate() { return main_isolate(); }
  Graph* graph() const { return main_graph_; }
  Zone* zone() const { return graph()->zone(); }
  CommonOperatorBuilder* common() { return &main_common_; }
  MachineOperatorBuilder* machine() { return &main_machine_; }

  void Build(const byte* start, const byte* end) {
    TreeResult result = BuildTFGraph(&jsgraph, &env, start, end);
    if (result.failed()) {
      ptrdiff_t pc = result.error_pc - result.start;
      ptrdiff_t pt = result.error_pt - result.start;
      std::ostringstream str;
      str << "Verification failed: " << result.error_code << " pc = +" << pc;
      if (result.error_pt) str << ", pt = +" << pt;
      str << ", msg = " << result.error_msg.get();
      FATAL(str.str().c_str());
    }
    if (FLAG_trace_turbo_graph) {
      OFStream os(stdout);
      os << AsRPO(*jsgraph.graph());
    }
  }

  byte AllocateLocal(LocalType type) {
    int result = static_cast<int>(env.total_locals);
    env.AddLocals(type, 1);
    byte b = static_cast<byte>(result);
    CHECK_EQ(result, b);
    return b;
  }

  Handle<Code> Compile(ModuleEnv* module) {
    CallDescriptor* desc = module->GetWasmCallDescriptor(this->zone(), env.sig);
    Handle<Code> result =
        Pipeline::GenerateCodeForTesting(this->isolate(), desc, this->graph());
    if (!result.is_null() && FLAG_print_opt_code) {
      OFStream os(stdout);
      result->Disassemble("wasm code", os);
    }

    return result;
  }

  unsigned CompileAndAdd(TestingModule* module) {
    unsigned index = 0;
    if (module->module && module->module->functions) {
      index = static_cast<unsigned>(module->module->functions->size());
    }
    module->AddFunction(env.sig, Compile(module));
    return index;
  }
};


#define BUILD(r, ...)                      \
  do {                                     \
    byte code[] = {__VA_ARGS__};           \
    r.Build(code, code + arraysize(code)); \
  } while (false)


TEST(Run_WasmInt8Const) {
  WasmRunner<int8_t> r;
  const byte kExpectedValue = 121;
  // return(kExpectedValue)
  BUILD(r, WASM_RETURN(WASM_INT8(kExpectedValue)));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt8Const_fallthru1) {
  WasmRunner<int8_t> r;
  const byte kExpectedValue = 122;
  // kExpectedValue
  BUILD(r, WASM_INT8(kExpectedValue));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt8Const_fallthru2) {
  WasmRunner<int8_t> r;
  const byte kExpectedValue = 123;
  // -99 kExpectedValue
  BUILD(r, WASM_INT8(-99), WASM_INT8(kExpectedValue));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt8Const_comma1) {
  WasmRunner<int8_t> r;
  const byte kExpectedValue = 124;
  // -98, kExpectedValue
  BUILD(r, WASM_COMMA(WASM_INT8(-98), WASM_INT8(kExpectedValue)));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt8Const_all) {
  for (int value = -128; value <= 127; value++) {
    WasmRunner<int8_t> r;
    // return(value)
    BUILD(r, WASM_RETURN(WASM_INT8(value)));
    int8_t result = r.Call();
    CHECK_EQ(value, result);
  }
}


TEST(Run_WasmInt32Const) {
  WasmRunner<int32_t> r;
  const int32_t kExpectedValue = 0x11223344;
  // return(kExpectedValue)
  BUILD(r, WASM_RETURN(WASM_INT32(kExpectedValue)));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt32Const_many) {
  FOR_INT32_INPUTS(i) {
    WasmRunner<int32_t> r;
    const int32_t kExpectedValue = *i;
    // return(kExpectedValue)
    BUILD(r, WASM_RETURN(WASM_INT32(kExpectedValue)));
    CHECK_EQ(kExpectedValue, r.Call());
  }
}


#if WASM_64
TEST(Run_WasmInt64Const) {
  WasmRunner<int64_t> r;
  const int64_t kExpectedValue = 0x1122334455667788LL;
  // return(kExpectedValue)
  r.function_env = &r.env_l_ll;
  BUILD(r, WASM_RETURN(WASM_INT64(kExpectedValue)));
  CHECK_EQ(kExpectedValue, r.Call());
}


TEST(Run_WasmInt64Const_many) {
  int cntr = 0;
  FOR_INT32_INPUTS(i) {
    WasmRunner<int64_t> r;
    r.function_env = &r.env_l_ll;
    const int64_t kExpectedValue = (static_cast<int64_t>(*i) << 32) | cntr;
    // return(kExpectedValue)
    BUILD(r, WASM_RETURN(WASM_INT64(kExpectedValue)));
    CHECK_EQ(kExpectedValue, r.Call());
    cntr++;
  }
}
#endif


TEST(Run_WasmInt32Param0) {
  WasmRunner<int32_t> r(kMachInt32);
  // return(local[0])
  BUILD(r, WASM_RETURN(WASM_GET_LOCAL(0)));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(*i)); }
}


TEST(Run_WasmInt32Param0_fallthru) {
  WasmRunner<int32_t> r(kMachInt32);
  // local[0]
  BUILD(r, WASM_GET_LOCAL(0));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(*i)); }
}


TEST(Run_WasmInt32Param1) {
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  // return(local[1])
  BUILD(r, WASM_RETURN(WASM_GET_LOCAL(1)));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i, r.Call(-111, *i)); }
}


TEST(Run_WasmInt32Add) {
  WasmRunner<int32_t> r;
  // return 11 + 44
  BUILD(r, WASM_RETURN(WASM_INT32_ADD(WASM_INT8(11), WASM_INT8(44))));
  CHECK_EQ(55, r.Call());
}


TEST(Run_WasmInt32Add_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // return p0 + 13
  BUILD(r, WASM_RETURN(WASM_INT32_ADD(WASM_INT8(13), WASM_GET_LOCAL(0))));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i + 13, r.Call(*i)); }
}


TEST(Run_WasmInt32Add_P_fallthru) {
  WasmRunner<int32_t> r(kMachInt32);
  // p0 + 13
  BUILD(r, WASM_INT32_ADD(WASM_INT8(13), WASM_GET_LOCAL(0)));
  FOR_INT32_INPUTS(i) { CHECK_EQ(*i + 13, r.Call(*i)); }
}


TEST(Run_WasmInt32Add_P2) {
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  // return p0 + p1
  BUILD(r, WASM_RETURN(WASM_INT32_ADD(WASM_GET_LOCAL(0), WASM_GET_LOCAL(1))));
  FOR_INT32_INPUTS(i) {
    FOR_INT32_INPUTS(j) {
      int32_t expected = static_cast<int32_t>(static_cast<uint32_t>(*i) +
                                              static_cast<uint32_t>(*j));
      CHECK_EQ(expected, r.Call(*i, *j));
    }
  }
}


TEST(Run_WasmFloat32Add) {
  WasmRunner<int32_t> r;
  // return int(11.5f + 44.5f)
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT32(
               WASM_FLOAT32_ADD(WASM_FLOAT32(11.5f), WASM_FLOAT32(44.5f)))));
  CHECK_EQ(56, r.Call());
}


TEST(Run_WasmFloat64Add) {
  WasmRunner<int32_t> r;
  // return int(13.5d + 43.5d)
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT64(
               WASM_FLOAT64_ADD(WASM_FLOAT64(13.5), WASM_FLOAT64(43.5)))));
  CHECK_EQ(57, r.Call());
}


void TestInt32Binop(WasmOpcode opcode, int32_t expected, int32_t a, int32_t b) {
  {
    WasmRunner<int32_t> r;
    // return K op K
    BUILD(r, WASM_RETURN(WASM_BINOP(opcode, WASM_INT32(a), WASM_INT32(b))));
    CHECK_EQ(expected, r.Call());
  }
  {
    WasmRunner<int32_t> r(kMachInt32, kMachInt32);
    // return a op b
    BUILD(r, WASM_RETURN(
                 WASM_BINOP(opcode, WASM_GET_LOCAL(0), WASM_GET_LOCAL(1))));
    CHECK_EQ(expected, r.Call(a, b));
  }
}


TEST(Run_WasmInt32Binops) {
  TestInt32Binop(kExprInt32Add, 88888888, 33333333, 55555555);
  TestInt32Binop(kExprInt32Sub, -1111111, 7777777, 8888888);
  TestInt32Binop(kExprInt32Mul, 65130756, 88734, 734);
  TestInt32Binop(kExprInt32SDiv, -66, -4777344, 72384);
  TestInt32Binop(kExprInt32UDiv, 805306368, 0xF0000000, 5);
  TestInt32Binop(kExprInt32SRem, -3, -3003, 1000);
  TestInt32Binop(kExprInt32URem, 4, 4004, 1000);
  TestInt32Binop(kExprInt32And, 0xEE, 0xFFEE, 0xFF0000FF);
  TestInt32Binop(kExprInt32Ior, 0xF0FF00FF, 0xF0F000EE, 0x000F0011);
  TestInt32Binop(kExprInt32Xor, 0xABCDEF01, 0xABCDEFFF, 0xFE);
  TestInt32Binop(kExprInt32Shl, 0xA0000000, 0xA, 28);
  TestInt32Binop(kExprInt32Shr, 0x07000010, 0x70000100, 4);
  TestInt32Binop(kExprInt32Sar, 0xFF000000, 0x80000000, 7);
  TestInt32Binop(kExprInt32Eq, 1, -99, -99);

  TestInt32Binop(kExprInt32Slt, 1, -4, 4);
  TestInt32Binop(kExprInt32Sle, 0, -2, -3);
  TestInt32Binop(kExprInt32Ult, 1, 0, -6);
  TestInt32Binop(kExprInt32Ule, 1, 98978, 0xF0000000);

  TestInt32Binop(kExprInt32Sgt, 1, 4, -4);
  TestInt32Binop(kExprInt32Sge, 0, -3, -2);
  TestInt32Binop(kExprInt32Ugt, 1, -6, 0);
  TestInt32Binop(kExprInt32Uge, 1, 0xF0000000, 98978);
}


#if WASM_64
void TestInt64Binop(WasmOpcode opcode, int64_t expected, int64_t a, int64_t b) {
  if (!WasmOpcodes::IsSupported(opcode)) return;
  {
    WasmRunner<int64_t> r;
    r.function_env = &r.env_l_ll;
    // return K op K
    BUILD(r, WASM_RETURN(WASM_BINOP(opcode, WASM_INT64(a), WASM_INT64(b))));
    CHECK_EQ(expected, r.Call());
  }
  {
    WasmRunner<int64_t> r(kMachInt64, kMachInt64);
    r.function_env = &r.env_l_ll;
    // return a op b
    BUILD(r, WASM_RETURN(
                 WASM_BINOP(opcode, WASM_GET_LOCAL(0), WASM_GET_LOCAL(1))));
    CHECK_EQ(expected, r.Call(a, b));
  }
}


TEST(Run_WasmInt64Binops) {
  // TODO(titzer): real 64-bit numbers
  TestInt64Binop(kExprInt64Add, 8888888888888LL, 3333333333333LL,
                 5555555555555LL);
  TestInt64Binop(kExprInt64Sub, -111111111111LL, 777777777777LL,
                 888888888888LL);
  TestInt64Binop(kExprInt64Mul, 65130756, 88734, 734);
  TestInt64Binop(kExprInt64SDiv, -66, -4777344, 72384);
  TestInt64Binop(kExprInt64UDiv, 805306368, 0xF0000000, 5);
  TestInt64Binop(kExprInt64SRem, -3, -3003, 1000);
  TestInt64Binop(kExprInt64URem, 4, 4004, 1000);
  TestInt64Binop(kExprInt64And, 0xEE, 0xFFEE, 0xFF0000FF);
  TestInt64Binop(kExprInt64Ior, 0xF0FF00FF, 0xF0F000EE, 0x000F0011);
  TestInt64Binop(kExprInt64Xor, 0xABCDEF01, 0xABCDEFFF, 0xFE);
  TestInt64Binop(kExprInt64Shl, 0xA0000000, 0xA, 28);
  TestInt64Binop(kExprInt64Shr, 0x0700001000123456LL, 0x7000010001234567LL, 4);
  TestInt64Binop(kExprInt64Sar, 0xFF00000000000000LL, 0x8000000000000000LL, 7);
  TestInt64Binop(kExprInt64Eq, 1, -99, -99);
  TestInt64Binop(kExprInt64Slt, 1, -4, 4);
  TestInt64Binop(kExprInt64Sle, 0, -2, -3);
  TestInt64Binop(kExprInt64Ult, 1, 0, -6);
  TestInt64Binop(kExprInt64Ule, 1, 98978, 0xF0000000);
}
#endif


void TestFloat32Binop(WasmOpcode opcode, int32_t expected, float a, float b) {
  WasmRunner<int32_t> r;
  // return K op K
  BUILD(r, WASM_RETURN(WASM_BINOP(opcode, WASM_FLOAT32(a), WASM_FLOAT32(b))));
  CHECK_EQ(expected, r.Call());
  // TODO(titzer): test float parameters
}


void TestFloat32BinopWithConvert(WasmOpcode opcode, int32_t expected, float a,
                                 float b) {
  WasmRunner<int32_t> r;
  // return int(K op K)
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT32(
               WASM_BINOP(opcode, WASM_FLOAT32(a), WASM_FLOAT32(b)))));
  CHECK_EQ(expected, r.Call());
  // TODO(titzer): test float parameters
}


void TestFloat32UnopWithConvert(WasmOpcode opcode, int32_t expected, float a) {
  WasmRunner<int32_t> r;
  // return int(K op K)
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT32(
               WASM_UNOP(opcode, WASM_FLOAT32(a)))));
  CHECK_EQ(expected, r.Call());
  // TODO(titzer): test float parameters
}


void TestFloat64Binop(WasmOpcode opcode, int32_t expected, double a, double b) {
  WasmRunner<int32_t> r;
  // return K op K
  BUILD(r, WASM_RETURN(WASM_BINOP(opcode, WASM_FLOAT64(a), WASM_FLOAT64(b))));
  CHECK_EQ(expected, r.Call());
  // TODO(titzer): test double parameters
}


void TestFloat64BinopWithConvert(WasmOpcode opcode, int32_t expected, double a,
                                 double b) {
  WasmRunner<int32_t> r;
  // return int(K op K)
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT64(
               WASM_BINOP(opcode, WASM_FLOAT64(a), WASM_FLOAT64(b)))));
  CHECK_EQ(expected, r.Call());
  // TODO(titzer): test double parameters
}


void TestFloat64UnopWithConvert(WasmOpcode opcode, int32_t expected, double a) {
  WasmRunner<int32_t> r;
  // return int(K op K)
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT64(
               WASM_UNOP(opcode, WASM_FLOAT64(a)))));
  CHECK_EQ(expected, r.Call());
  // TODO(titzer): test float parameters
}


TEST(Run_WasmFloat32Binops) {
  TestFloat32Binop(kExprFloat32Eq, 1, 8.125, 8.125);
  TestFloat32Binop(kExprFloat32Lt, 1, -9.5, -9);
  TestFloat32Binop(kExprFloat32Le, 1, -1111, -1111);
  TestFloat32Binop(kExprFloat32Gt, 1, -9, -9.5);
  TestFloat32Binop(kExprFloat32Ge, 1, -1111, -1111);

  TestFloat32BinopWithConvert(kExprFloat32Add, 10, 3.5, 6.5);
  TestFloat32BinopWithConvert(kExprFloat32Sub, 2, 44.5, 42.5);
  TestFloat32BinopWithConvert(kExprFloat32Mul, -66, -132.1, 0.5);
  TestFloat32BinopWithConvert(kExprFloat32Div, 11, 22.1, 2);
}


TEST(Run_WasmFloat32Unops) {
  TestFloat32UnopWithConvert(kExprFloat32Abs, 8, 8.125);
  TestFloat32UnopWithConvert(kExprFloat32Abs, 9, -9.125);
  TestFloat32UnopWithConvert(kExprFloat32Neg, -213, 213.125);
}


TEST(Run_WasmFloat64Binops) {
  TestFloat64Binop(kExprFloat64Eq, 1, 16.25, 16.25);
  TestFloat64Binop(kExprFloat64Lt, 1, -32.4, 11.7);
  TestFloat64Binop(kExprFloat64Le, 1, -88.9, -88.9);
  TestFloat64Binop(kExprFloat64Gt, 1, 11.7, -32.4);
  TestFloat64Binop(kExprFloat64Ge, 1, -88.9, -88.9);

  TestFloat64BinopWithConvert(kExprFloat64Add, 100, 43.5, 56.5);
  TestFloat64BinopWithConvert(kExprFloat64Sub, 200, 12200.1, 12000.1);
  TestFloat64BinopWithConvert(kExprFloat64Mul, -33, 134, -0.25);
  TestFloat64BinopWithConvert(kExprFloat64Div, -1111, -2222.3, 2);
}


TEST(Run_WasmFloat64Unops) {
  TestFloat64UnopWithConvert(kExprFloat64Abs, 108, 108.125);
  TestFloat64UnopWithConvert(kExprFloat64Abs, 209, -209.125);
  TestFloat64UnopWithConvert(kExprFloat64Neg, -209, 209.125);
}


TEST(Run_Wasm_IfThen_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // if (p0) return 11; else return 22;
  BUILD(r, WASM_IF_THEN(WASM_GET_LOCAL(0),             // --
                        WASM_RETURN(WASM_INT8(11)),    // --
                        WASM_RETURN(WASM_INT8(22))));  // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 11 : 22;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_VoidReturn) {
  WasmRunner<void> r;
  r.function_env = &r.env_v_v;
  BUILD(r, WASM_RETURN0);
  r.Call();
}


TEST(Run_Wasm_Block_If_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // { if (p0) return 51; return 52; }
  BUILD(r, WASM_BLOCK(2,                                    // --
                      WASM_IF(WASM_GET_LOCAL(0),            // --
                              WASM_RETURN(WASM_INT8(51))),  // --
                      WASM_RETURN(WASM_INT8(52))));         // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 51 : 52;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Block_IfThen_P_assign) {
  WasmRunner<int32_t> r(kMachInt32);
  // { if (p0) p0 = 71; else p0 = 72; return p0; }
  BUILD(r, WASM_BLOCK(2,                                               // --
                      WASM_IF_THEN(WASM_GET_LOCAL(0),                  // --
                                   WASM_SET_LOCAL(0, WASM_INT8(71)),   // --
                                   WASM_SET_LOCAL(0, WASM_INT8(72))),  // --
                      WASM_RETURN(WASM_GET_LOCAL(0))));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 71 : 72;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Block_If_P_assign) {
  WasmRunner<int32_t> r(kMachInt32);
  // { if (p0) p0 = 61; return p0; }
  BUILD(r, WASM_BLOCK(
               2, WASM_IF(WASM_GET_LOCAL(0), WASM_SET_LOCAL(0, WASM_INT8(61))),
               WASM_RETURN(WASM_GET_LOCAL(0))));
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 61 : *i;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Ternary_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // return p0 ? 11 : 22;
  BUILD(r, WASM_RETURN(WASM_TERNARY(WASM_GET_LOCAL(0),  // --
                                    WASM_INT8(11),      // --
                                    WASM_INT8(22))));   // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 11 : 22;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Ternary_P_fallthru) {
  WasmRunner<int32_t> r(kMachInt32);
  // p0 ? 11 : 22;
  BUILD(r, WASM_TERNARY(WASM_GET_LOCAL(0),  // --
                        WASM_INT8(11),      // --
                        WASM_INT8(22)));    // --
  FOR_INT32_INPUTS(i) {
    int32_t expected = *i ? 11 : 22;
    CHECK_EQ(expected, r.Call(*i));
  }
}


TEST(Run_Wasm_Comma_P) {
  WasmRunner<int32_t> r(kMachInt32);
  // return p0, 17;
  BUILD(r, WASM_RETURN(WASM_COMMA(WASM_GET_LOCAL(0), WASM_INT8(17))));
  FOR_INT32_INPUTS(i) { CHECK_EQ(17, r.Call(*i)); }
}


TEST(Run_Wasm_CountDown) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r,
        WASM_BLOCK(
            2, WASM_LOOP(2, WASM_IF(WASM_NOT(WASM_GET_LOCAL(0)), WASM_BREAK(0)),
                         WASM_SET_LOCAL(0, WASM_INT32_SUB(WASM_GET_LOCAL(0),
                                                          WASM_INT8(1)))),
            WASM_RETURN(WASM_GET_LOCAL(0))));
  CHECK_EQ(0, r.Call(1));
  CHECK_EQ(0, r.Call(10));
  CHECK_EQ(0, r.Call(100));
}


TEST(Run_Wasm_CountDown_fallthru) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r,
        WASM_BLOCK(
            2, WASM_LOOP(2, WASM_IF(WASM_NOT(WASM_GET_LOCAL(0)), WASM_BREAK(0)),
                         WASM_SET_LOCAL(0, WASM_INT32_SUB(WASM_GET_LOCAL(0),
                                                          WASM_INT8(1)))),
            WASM_GET_LOCAL(0)));
  CHECK_EQ(0, r.Call(1));
  CHECK_EQ(0, r.Call(10));
  CHECK_EQ(0, r.Call(100));
}


TEST(Run_Wasm_WhileCountDown) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_BLOCK(
               2, WASM_WHILE(WASM_GET_LOCAL(0),
                             WASM_SET_LOCAL(0, WASM_INT32_SUB(WASM_GET_LOCAL(0),
                                                              WASM_INT8(1)))),
               WASM_RETURN(WASM_GET_LOCAL(0))));
  CHECK_EQ(0, r.Call(1));
  CHECK_EQ(0, r.Call(10));
  CHECK_EQ(0, r.Call(100));
}


TEST(Run_Wasm_Loop_if_break1) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_BLOCK(2, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(0)),
                                   WASM_SET_LOCAL(0, WASM_INT8(99))),
                      WASM_RETURN(WASM_GET_LOCAL(0))));
  CHECK_EQ(99, r.Call(0));
  CHECK_EQ(3, r.Call(3));
  CHECK_EQ(10000, r.Call(10000));
  CHECK_EQ(-29, r.Call(-29));
}


TEST(Run_Wasm_Loop_if_break_fallthru) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_BLOCK(1, WASM_LOOP(2, WASM_IF(WASM_GET_LOCAL(0), WASM_BREAK(1)),
                                   WASM_SET_LOCAL(0, WASM_INT8(93)))),
        WASM_GET_LOCAL(0));
  CHECK_EQ(93, r.Call(0));
  CHECK_EQ(3, r.Call(3));
  CHECK_EQ(10001, r.Call(10001));
  CHECK_EQ(-22, r.Call(-22));
}


TEST(Run_Wasm_LoadMemInt32) {
  WasmRunner<int32_t> r(kMachInt32);
  TestingModule module;
  int32_t* memory = module.AddMemoryElems<int32_t>(8);
  module.RandomizeMemory(1111);
  r.function_env->module = &module;

  BUILD(r, WASM_RETURN(WASM_LOAD_MEM(kMemInt32, WASM_INT8(0))));

  memory[0] = 99999999;
  CHECK_EQ(99999999, r.Call(0));

  memory[0] = 88888888;
  CHECK_EQ(88888888, r.Call(0));

  memory[0] = 77777777;
  CHECK_EQ(77777777, r.Call(0));
}


TEST(Run_Wasm_LoadMemInt32_P) {
  const int kNumElems = 8;
  WasmRunner<int32_t> r(kMachInt32);
  TestingModule module;
  int32_t* memory = module.AddMemoryElems<int32_t>(kNumElems);
  module.RandomizeMemory(2222);
  r.function_env->module = &module;

  BUILD(r, WASM_RETURN(WASM_LOAD_MEM(kMemInt32, WASM_GET_LOCAL(0))));

  for (int i = 0; i < kNumElems; i++) {
    CHECK_EQ(memory[i], r.Call(i * 4));
  }
}


TEST(Run_Wasm_MemInt32_Sum) {
  WasmRunner<uint32_t> r(kMachInt32);
  const int kNumElems = 20;
  const byte kSum = r.AllocateLocal(kAstInt32);
  TestingModule module;
  uint32_t* memory = module.AddMemoryElems<uint32_t>(kNumElems);
  r.function_env->module = &module;

  BUILD(
      r,
      WASM_BLOCK(
          2, WASM_WHILE(
                 WASM_GET_LOCAL(0),
                 WASM_BLOCK(2, WASM_SET_LOCAL(
                                   kSum, WASM_INT32_ADD(
                                             WASM_GET_LOCAL(kSum),
                                             WASM_LOAD_MEM(kMemInt32,
                                                           WASM_GET_LOCAL(0)))),
                            WASM_SET_LOCAL(0, WASM_INT32_SUB(WASM_GET_LOCAL(0),
                                                             WASM_INT8(4))))),
          WASM_RETURN(WASM_GET_LOCAL(1))));

  // Run 4 trials.
  for (int i = 0; i < 3; i++) {
    module.RandomizeMemory(i * 33);
    uint32_t expected = 0;
    for (size_t j = kNumElems - 1; j > 0; j--) {
      expected += memory[j];
    }
    uint32_t result = r.Call(static_cast<int>(4 * (kNumElems - 1)));
    CHECK_EQ(expected, result);
  }
}


TEST(Run_Wasm_MemFloat32_Sum) {
  WasmRunner<int32_t> r(kMachInt32);
  const byte kSum = r.AllocateLocal(kAstFloat32);
  ModuleEnv module;
  const int kSize = 5;
  float buffer[kSize] = {-99.25, -888.25, -77.25, 66666.25, 5555.25};
  module.mem_start = reinterpret_cast<uintptr_t>(&buffer);
  module.mem_end = reinterpret_cast<uintptr_t>(&buffer[kSize]);
  r.function_env->module = &module;

  BUILD(
      r,
      WASM_BLOCK(
          3, WASM_WHILE(
                 WASM_GET_LOCAL(0),
                 WASM_BLOCK(2, WASM_SET_LOCAL(
                                   kSum, WASM_FLOAT32_ADD(
                                             WASM_GET_LOCAL(kSum),
                                             WASM_LOAD_MEM(kMemFloat32,
                                                           WASM_GET_LOCAL(0)))),
                            WASM_SET_LOCAL(0, WASM_INT32_SUB(WASM_GET_LOCAL(0),
                                                             WASM_INT8(4))))),
          WASM_STORE_MEM(kMemFloat32, WASM_ZERO, WASM_GET_LOCAL(kSum)),
          WASM_RETURN(WASM_GET_LOCAL(0))));

  CHECK_EQ(0, r.Call(4 * (kSize - 1)));
  CHECK_NE(-99.25, buffer[0]);
  CHECK_EQ(71256.0f, buffer[0]);
}


template <typename T>
void GenerateAndRunFold(WasmOpcode binop, T* buffer, size_t size,
                        LocalType astType, MemType memType) {
  WasmRunner<int32_t> r(kMachInt32);
  const byte kAccum = r.AllocateLocal(astType);
  ModuleEnv module;
  module.mem_start = reinterpret_cast<uintptr_t>(buffer);
  module.mem_end = reinterpret_cast<uintptr_t>(buffer + size);
  r.function_env->module = &module;

  BUILD(r,
        WASM_BLOCK(
            4, WASM_SET_LOCAL(kAccum, WASM_LOAD_MEM(memType, WASM_ZERO)),
            WASM_WHILE(
                WASM_GET_LOCAL(0),
                WASM_BLOCK(
                    2, WASM_SET_LOCAL(
                           kAccum, WASM_BINOP(binop, WASM_GET_LOCAL(kAccum),
                                              WASM_LOAD_MEM(
                                                  memType, WASM_GET_LOCAL(0)))),
                    WASM_SET_LOCAL(0, WASM_INT32_SUB(WASM_GET_LOCAL(0),
                                                     WASM_INT8(sizeof(T)))))),
            WASM_STORE_MEM(memType, WASM_ZERO, WASM_GET_LOCAL(kAccum)),
            WASM_RETURN(WASM_GET_LOCAL(0))));
  r.Call(static_cast<int>(sizeof(T) * (size - 1)));
}


TEST(Run_Wasm_MemFloat64_Mul) {
  const size_t kSize = 6;
  double buffer[kSize] = {1, 2, 2, 2, 2, 2};
  GenerateAndRunFold<double>(kExprFloat64Mul, buffer, kSize, kAstFloat64,
                             kMemFloat64);
  CHECK_EQ(32, buffer[0]);
}


TEST(Run_Wasm_Switch0) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_BLOCK(2, WASM_ID(kStmtSwitch, 0, WASM_GET_LOCAL(0)),
                      WASM_RETURN(WASM_GET_LOCAL(0))));
  CHECK_EQ(0, r.Call(0));
  CHECK_EQ(1, r.Call(1));
  CHECK_EQ(2, r.Call(2));
  CHECK_EQ(32, r.Call(32));
}


TEST(Run_Wasm_Switch1) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_BLOCK(2, WASM_SWITCH(1, WASM_GET_LOCAL(0),
                                     WASM_SET_LOCAL(0, WASM_INT8(44))),
                      WASM_RETURN(WASM_GET_LOCAL(0))));
  CHECK_EQ(44, r.Call(0));
  CHECK_EQ(1, r.Call(1));
  CHECK_EQ(2, r.Call(2));
  CHECK_EQ(-834, -834);
}


TEST(Run_Wasm_Switch4_fallthru) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_BLOCK(2, WASM_SWITCH(4,                            // --
                                     WASM_GET_LOCAL(0),            // key
                                     WASM_NOP,                     // case 0
                                     WASM_RETURN(WASM_INT8(45)),   // case 1
                                     WASM_NOP,                     // case 2
                                     WASM_RETURN(WASM_INT8(47))),  // case 3
                      WASM_RETURN(WASM_GET_LOCAL(0))));

  CHECK_EQ(-1, r.Call(-1));
  CHECK_EQ(45, r.Call(0));
  CHECK_EQ(45, r.Call(1));
  CHECK_EQ(47, r.Call(2));
  CHECK_EQ(47, r.Call(3));
  CHECK_EQ(4, r.Call(4));
  CHECK_EQ(-834, -834);
}


#define APPEND(code, pos, fragment)                 \
  do {                                              \
    memcpy(code + pos, fragment, sizeof(fragment)); \
    pos += sizeof(fragment);                        \
  } while (false)


TEST(Run_Wasm_Switch_Ret_N) {
  Zone zone;
  for (int i = 3; i < 256; i += 28) {
    byte header[] = {kStmtBlock, 2, kStmtSwitch, static_cast<byte>(i),
                     kExprGetLocal, 0};
    byte ccase[] = {WASM_RETURN(WASM_INT32(i))};
    byte footer[] = {WASM_RETURN(WASM_GET_LOCAL(0))};
    byte* code = zone.NewArray<byte>(sizeof(header) + i * sizeof(ccase) +
                                     sizeof(footer));
    int pos = 0;
    // Add header code.
    APPEND(code, pos, header);
    for (int j = 0; j < i; j++) {
      // Add case code.
      byte ccase[] = {WASM_RETURN(WASM_INT32((10 + j)))};
      APPEND(code, pos, ccase);
    }
    // Add footer code.
    APPEND(code, pos, footer);
    // Build graph.
    WasmRunner<int32_t> r(kMachInt32);
    r.Build(code, code + pos);
    // Run.
    for (int j = -1; j < i + 5; j++) {
      int expected = j >= 0 && j < i ? (10 + j) : j;
      CHECK_EQ(expected, r.Call(j));
    }
  }
}


TEST(Run_Wasm_Switch_Nf_N) {
  Zone zone;
  for (int i = 3; i < 256; i += 28) {
    byte header[] = {kStmtBlock, 2, kStmtSwitchNf, static_cast<byte>(i),
                     kExprGetLocal, 0};
    byte ccase[] = {WASM_SET_LOCAL(0, WASM_INT32(i))};
    byte footer[] = {WASM_RETURN(WASM_GET_LOCAL(0))};
    byte* code = zone.NewArray<byte>(sizeof(header) + i * sizeof(ccase) +
                                     sizeof(footer));
    int pos = 0;
    // Add header code.
    APPEND(code, pos, header);
    for (int j = 0; j < i; j++) {
      // Add case code.
      byte ccase[] = {WASM_SET_LOCAL(0, WASM_INT32((10 + j)))};
      APPEND(code, pos, ccase);
    }
    // Add footer code.
    APPEND(code, pos, footer);
    // Build graph.
    WasmRunner<int32_t> r(kMachInt32);
    r.Build(code, code + pos);
    // Run.
    for (int j = -1; j < i + 5; j++) {
      int expected = j >= 0 && j < i ? (10 + j) : j;
      CHECK_EQ(expected, r.Call(j));
    }
  }
}


TEST(Build_Wasm_Infinite_Loop) {
  WasmRunner<int32_t> r(kMachInt32);
  // Only build the graph and compile, don't run.
  BUILD(r, WASM_INFINITE_LOOP);
  r.GenerateCode();
}


TEST(Run_Wasm_Infinite_Loop_not_taken1) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_IF_THEN(WASM_GET_LOCAL(0), WASM_INFINITE_LOOP,
                        WASM_RETURN(WASM_INT8(45))));
  // Run the code, but don't go into the infinite loop.
  CHECK_EQ(45, r.Call(0));
}


TEST(Run_Wasm_Infinite_Loop_not_taken2) {
  WasmRunner<int32_t> r(kMachInt32);
  BUILD(r, WASM_IF_THEN(WASM_GET_LOCAL(0), WASM_RETURN(WASM_INT8(45)),
                        WASM_INFINITE_LOOP));
  // Run the code, but don't go into the infinite loop.
  CHECK_EQ(45, r.Call(1));
}


static void TestBuildGraphForUnop(WasmOpcode opcode, FunctionSig* sig) {
  WasmRunner<int32_t> r(kMachInt32);
  init_env(r.function_env, sig);
  BUILD(r, kStmtReturn, static_cast<byte>(opcode), kExprGetLocal, 0);
}


static void TestBuildGraphForBinop(WasmOpcode opcode, FunctionSig* sig) {
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  init_env(r.function_env, sig);
  BUILD(r, kStmtReturn, static_cast<byte>(opcode), kExprGetLocal, 0,
        kExprGetLocal, 1);
}


TEST(Build_Wasm_SimpleExprs) {
// Test that the decoder can build a graph for all supported simple expressions.
#define GRAPH_BUILD_TEST(name, opcode, sig)                 \
  if (WasmOpcodes::IsSupported(kExpr##name)) {              \
    FunctionSig* sig = WasmOpcodes::Signature(kExpr##name); \
    if (sig->parameter_count() == 1) {                      \
      TestBuildGraphForUnop(kExpr##name, sig);              \
    } else {                                                \
      TestBuildGraphForBinop(kExpr##name, sig);             \
    }                                                       \
  }

  FOREACH_SIMPLE_EXPR_OPCODE(GRAPH_BUILD_TEST);

#undef GRAPH_BUILD_TEST
}


TEST(Run_Wasm_Int32LoadInt8_signext) {
  TestingModule module;
  const int kNumElems = 16;
  int8_t* memory = module.AddMemoryElems<int8_t>(kNumElems);
  module.RandomizeMemory();
  memory[0] = -1;
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_LOAD_MEM(kMemInt8, WASM_GET_LOCAL(0))));

  for (size_t i = 0; i < kNumElems; i++) {
    CHECK_EQ(memory[i], r.Call(static_cast<int>(i)));
  }
}


TEST(Run_Wasm_Int32LoadInt8_zeroext) {
  TestingModule module;
  const int kNumElems = 16;
  byte* memory = module.AddMemory(kNumElems);
  module.RandomizeMemory(77);
  memory[0] = 255;
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_LOAD_MEM(kMemUint8, WASM_GET_LOCAL(0))));

  for (size_t i = 0; i < kNumElems; i++) {
    CHECK_EQ(memory[i], r.Call(static_cast<int>(i)));
  }
}


TEST(Run_Wasm_Int32LoadInt16_signext) {
  TestingModule module;
  const int kNumBytes = 16;
  byte* memory = module.AddMemory(kNumBytes);
  module.RandomizeMemory(888);
  memory[1] = 200;
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_LOAD_MEM(kMemInt16, WASM_GET_LOCAL(0))));

  for (size_t i = 0; i < kNumBytes; i += 2) {
    int32_t expected = memory[i] | (static_cast<int8_t>(memory[i + 1]) << 8);
    CHECK_EQ(expected, r.Call(static_cast<int>(i)));
  }
}


TEST(Run_Wasm_Int32LoadInt16_zeroext) {
  TestingModule module;
  const int kNumBytes = 16;
  byte* memory = module.AddMemory(kNumBytes);
  module.RandomizeMemory(9999);
  memory[1] = 204;
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_LOAD_MEM(kMemUint16, WASM_GET_LOCAL(0))));

  for (size_t i = 0; i < kNumBytes; i += 2) {
    int32_t expected = memory[i] | (memory[i + 1] << 8);
    CHECK_EQ(expected, r.Call(static_cast<int>(i)));
  }
}


TEST(Run_WasmInt32Global) {
  TestingModule module;
  int32_t* global = module.AddGlobal<int32_t>(kMemInt32);
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  // global = global + p0
  BUILD(r, WASM_RETURN(WASM_STORE_GLOBAL(
               0, WASM_INT32_ADD(WASM_LOAD_GLOBAL(0), WASM_GET_LOCAL(0)))));

  *global = 116;
  for (int i = 9; i < 444444; i += 111111) {
    int32_t expected = *global + i;
    r.Call(i);
    CHECK_EQ(expected, *global);
  }
}


TEST(Run_WasmInt32Globals_DontAlias) {
  const int kNumGlobals = 3;
  TestingModule module;
  int32_t* globals[] = {module.AddGlobal<int32_t>(kMemInt32),
                        module.AddGlobal<int32_t>(kMemInt32),
                        module.AddGlobal<int32_t>(kMemInt32)};

  for (int g = 0; g < kNumGlobals; g++) {
    // global = global + p0
    WasmRunner<int32_t> r(kMachInt32);
    r.function_env->module = &module;
    BUILD(r, WASM_RETURN(WASM_STORE_GLOBAL(
                 g, WASM_INT32_ADD(WASM_LOAD_GLOBAL(g), WASM_GET_LOCAL(0)))));

    // Check that reading/writing global number {g} doesn't alter the others.
    *globals[g] = 116 * g;
    int32_t before[kNumGlobals];
    for (int i = 9; i < 444444; i += 111113) {
      int32_t sum = *globals[g] + i;
      for (int j = 0; j < kNumGlobals; j++) before[j] = *globals[j];
      r.Call(i);
      for (int j = 0; j < kNumGlobals; j++) {
        int32_t expected = j == g ? sum : before[j];
        CHECK_EQ(expected, *globals[j]);
      }
    }
  }
}


#if WASM_64
TEST(Run_WasmInt64Global) {
  TestingModule module;
  int64_t* global = module.AddGlobal<int64_t>(kMemInt64);
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  // global = global + p0
  BUILD(r, WASM_BLOCK(
               2, WASM_STORE_GLOBAL(0, WASM_INT64_ADD(WASM_LOAD_GLOBAL(0),
                                                      WASM_INT64_SCONVERT_INT32(
                                                          WASM_GET_LOCAL(0)))),
               WASM_RETURN(WASM_ZERO)));

  *global = 0xFFFFFFFFFFFFFFFFLL;
  for (int i = 9; i < 444444; i += 111111) {
    int64_t expected = *global + i;
    r.Call(i);
    CHECK_EQ(expected, *global);
  }
}
#endif


TEST(Run_WasmFloat32Global) {
  TestingModule module;
  float* global = module.AddGlobal<float>(kMemFloat32);
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  // global = global + p0
  BUILD(r, WASM_BLOCK(2, WASM_STORE_GLOBAL(
                             0, WASM_FLOAT32_ADD(WASM_LOAD_GLOBAL(0),
                                                 WASM_FLOAT32_SCONVERT_INT32(
                                                     WASM_GET_LOCAL(0)))),
                      WASM_RETURN(WASM_ZERO)));

  *global = 1.25;
  for (int i = 9; i < 4444; i += 1111) {
    volatile float expected = *global + i;
    r.Call(i);
    CHECK_EQ(expected, *global);
  }
}


TEST(Run_WasmFloat64Global) {
  TestingModule module;
  double* global = module.AddGlobal<double>(kMemFloat64);
  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;
  // global = global + p0
  BUILD(r, WASM_BLOCK(2, WASM_STORE_GLOBAL(
                             0, WASM_FLOAT64_ADD(WASM_LOAD_GLOBAL(0),
                                                 WASM_FLOAT64_SCONVERT_INT32(
                                                     WASM_GET_LOCAL(0)))),
                      WASM_RETURN(WASM_ZERO)));

  *global = 1.25;
  for (int i = 9; i < 4444; i += 1111) {
    volatile double expected = *global + i;
    r.Call(i);
    CHECK_EQ(expected, *global);
  }
}


TEST(Run_WasmMixedGlobals) {
  TestingModule module;
  int32_t* unused = module.AddGlobal<int32_t>(kMemInt32);
  byte* memory = module.AddMemory(32);

  int8_t* var_int8 = module.AddGlobal<int8_t>(kMemInt8);
  uint8_t* var_uint8 = module.AddGlobal<uint8_t>(kMemUint8);
  int16_t* var_int16 = module.AddGlobal<int16_t>(kMemInt16);
  uint16_t* var_uint16 = module.AddGlobal<uint16_t>(kMemUint16);
  int32_t* var_int32 = module.AddGlobal<int32_t>(kMemInt32);
  uint32_t* var_uint32 = module.AddGlobal<uint32_t>(kMemUint32);
  float* var_float = module.AddGlobal<float>(kMemFloat32);
  double* var_double = module.AddGlobal<double>(kMemFloat64);

  WasmRunner<int32_t> r(kMachInt32);
  r.function_env->module = &module;

  BUILD(r,
        WASM_BLOCK(9, WASM_STORE_GLOBAL(1, WASM_LOAD_MEM(kMemInt8, WASM_ZERO)),
                   WASM_STORE_GLOBAL(2, WASM_LOAD_MEM(kMemUint8, WASM_ZERO)),
                   WASM_STORE_GLOBAL(3, WASM_LOAD_MEM(kMemInt16, WASM_ZERO)),
                   WASM_STORE_GLOBAL(4, WASM_LOAD_MEM(kMemUint16, WASM_ZERO)),
                   WASM_STORE_GLOBAL(5, WASM_LOAD_MEM(kMemInt32, WASM_ZERO)),
                   WASM_STORE_GLOBAL(6, WASM_LOAD_MEM(kMemUint32, WASM_ZERO)),
                   WASM_STORE_GLOBAL(7, WASM_LOAD_MEM(kMemFloat32, WASM_ZERO)),
                   WASM_STORE_GLOBAL(8, WASM_LOAD_MEM(kMemFloat64, WASM_ZERO)),
                   WASM_RETURN(WASM_ZERO)));

  memory[0] = 0xaa;
  memory[1] = 0xcc;
  memory[2] = 0x55;
  memory[3] = 0xee;
  memory[4] = 0x33;
  memory[5] = 0x22;
  memory[6] = 0x11;
  memory[7] = 0x99;
  r.Call(1);

  CHECK(static_cast<int8_t>(0xaa) == *var_int8);
  CHECK(static_cast<uint8_t>(0xaa) == *var_uint8);
  CHECK(static_cast<int16_t>(0xccaa) == *var_int16);
  CHECK(static_cast<uint16_t>(0xccaa) == *var_uint16);
  CHECK(static_cast<int32_t>(0xee55ccaa) == *var_int32);
  CHECK(static_cast<uint32_t>(0xee55ccaa) == *var_uint32);
  CHECK(bit_cast<float>(0xee55ccaa) == *var_float);
  CHECK(bit_cast<double>(0x99112233ee55ccaaULL) == *var_double);

  USE(unused);
}


TEST(Run_WasmCallEmpty) {
  const int32_t kExpected = -414444;
  // Build the target function.
  TestSignatures sigs;
  TestingModule module;
  WasmFunctionCompiler t(sigs.i_v());
  BUILD(t, WASM_RETURN(WASM_INT32(kExpected)));
  unsigned index = t.CompileAndAdd(&module);

  // Build the calling function.
  WasmRunner<int32_t> r;
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_CALL_FUNCTION0(index)));

  int32_t result = r.Call();
  CHECK_EQ(kExpected, result);
}


TEST(Run_WasmCallVoid) {
  const byte kMemOffset = 8;
  const int32_t kElemNum = kMemOffset / sizeof(int32_t);
  const int32_t kExpected = -414444;
  // Build the target function.
  TestSignatures sigs;
  TestingModule module;
  module.AddMemory(16);
  module.RandomizeMemory();
  WasmFunctionCompiler t(sigs.v_v());
  t.env.module = &module;
  BUILD(t, WASM_STORE_MEM(kMemInt32, WASM_INT8(kMemOffset),
                          WASM_INT32(kExpected)));
  unsigned index = t.CompileAndAdd(&module);

  // Build the calling function.
  WasmRunner<int32_t> r;
  r.function_env->module = &module;
  BUILD(r, WASM_CALL_FUNCTION0(index),
        WASM_LOAD_MEM(kMemInt32, WASM_INT8(kMemOffset)));

  int32_t result = r.Call();
  CHECK_EQ(kExpected, result);
  CHECK_EQ(kExpected, module.raw_mem_start<int32_t>()[kElemNum]);
}


TEST(Run_WasmCall_Int32Add) {
  // Build the target function.
  TestSignatures sigs;
  TestingModule module;
  WasmFunctionCompiler t(sigs.i_ii());
  BUILD(t, WASM_RETURN(WASM_INT32_ADD(WASM_GET_LOCAL(0), WASM_GET_LOCAL(1))));
  unsigned index = t.CompileAndAdd(&module);

  // Build the caller function.
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_CALL_FUNCTION(index, WASM_GET_LOCAL(0),
                                          WASM_GET_LOCAL(1))));

  FOR_INT32_INPUTS(i) {
    FOR_INT32_INPUTS(j) {
      int32_t expected = static_cast<int32_t>(static_cast<uint32_t>(*i) +
                                              static_cast<uint32_t>(*j));
      CHECK_EQ(expected, r.Call(*i, *j));
    }
  }
}


TEST(Run_WasmCall_Float64Sub) {
  TestSignatures sigs;
  WasmFunctionCompiler t(sigs.d_dd());

  // Build the target function.
  TestingModule module;
  BUILD(t, WASM_RETURN(WASM_FLOAT64_SUB(WASM_GET_LOCAL(0), WASM_GET_LOCAL(1))));
  unsigned index = t.CompileAndAdd(&module);

  // Builder the caller function.
  WasmRunner<int32_t> r(kMachInt32, kMachInt32);
  r.function_env->module = &module;
  BUILD(r, WASM_RETURN(WASM_INT32_SCONVERT_FLOAT64(WASM_CALL_FUNCTION(
               index, WASM_FLOAT64_SCONVERT_INT32(WASM_GET_LOCAL(0)),
               WASM_FLOAT64_SCONVERT_INT32(WASM_GET_LOCAL(1))))));

  FOR_INT32_INPUTS(i) {
    FOR_INT32_INPUTS(j) {
      int32_t expected = static_cast<int32_t>(static_cast<double>(*i) -
                                              static_cast<double>(*j));
      CHECK_EQ(expected, r.Call(*i, *j));
    }
  }
}


//==========================================================
// TODO(titzer): move me to test-run-wasm-module.cc
//==========================================================
#define MODULE_HEADER(globals_count, functions_count, data_segments_count) \
  static_cast<uint8_t>(globals_count),                                     \
      static_cast<uint8_t>(globals_count >> 8),                            \
      static_cast<uint8_t>(functions_count),                               \
      static_cast<uint8_t>(functions_count >> 8),                          \
      static_cast<uint8_t>(data_segments_count),                           \
      static_cast<uint8_t>(data_segments_count >> 8)


TEST(Run_WasmModule_Return114) {
  static const byte kReturnValue = 114;
  static const byte kCodeStartOffset = 30;
  static const byte kCodeEndOffset = 33;
  static const byte data[] = {
      MODULE_HEADER(0, 1, 0),     // globals, functions, data segments
      0, kAstInt32,               // signature: void -> int
      0, 0, 0, 0,                 // name offset
      kCodeStartOffset, 0, 0, 0,  // code start offset
      kCodeEndOffset, 0, 0, 0,    // code end offset
      0, 0,                       // local int32 count
      0, 0,                       // local int64 count
      0, 0,                       // local float32 count
      0, 0,                       // local float64 count
      1,                          // exported
      0,                          // external
      kStmtReturn,                // body
      kExprInt8Const,             // --
      kReturnValue                // --
  };

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(kReturnValue, result);
}


TEST(Run_WasmModule_CallAdd) {
  static const int kModuleHeaderSize = 6;
  static const int kFunctionSize = 24;
  static const byte kCodeStartOffset0 =
      kModuleHeaderSize + 2 + kFunctionSize * 2;
  static const byte kCodeEndOffset0 = kCodeStartOffset0 + 6;
  static const byte kCodeStartOffset1 = kCodeEndOffset0;
  static const byte kCodeEndOffset1 = kCodeEndOffset0 + 7;
  static const byte data[] = {
      MODULE_HEADER(0, 2, 0),  // globals, functions, data segments
      // func#0 -----------------------------------------
      2, kAstInt32, kAstInt32, kAstInt32,  // signature: int,int -> int
      0, 0, 0, 0,                          // name offset
      kCodeStartOffset0, 0, 0, 0,          // code start offset
      kCodeEndOffset0, 0, 0, 0,            // code end offset
      0, 0,                                // local int32 count
      0, 0,                                // local int64 count
      0, 0,                                // local float32 count
      0, 0,                                // local float64 count
      0,                                   // exported
      0,                                   // external
      // func#1 (main) ----------------------------------
      0, kAstInt32,                // signature: void -> int
      0, 0, 0, 0,                  // name offset
      kCodeStartOffset1, 0, 0, 0,  // code start offset
      kCodeEndOffset1, 0, 0, 0,    // code end offset
      0, 0,                        // local int32 count
      0, 0,                        // local int64 count
      0, 0,                        // local float32 count
      0, 0,                        // local float64 count
      1,                           // exported
      0,                           // external
      // body#0 -----------------------------------------
      kStmtReturn,       // --
      kExprInt32Add,     // --
      kExprGetLocal, 0,  // --
      kExprGetLocal, 1,  // --
      // body#1 -----------------------------------------
      kStmtReturn,           // --
      kExprCallFunction, 0,  // --
      kExprInt8Const, 77,    // --
      kExprInt8Const, 22     // --
  };

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(99, result);
}


TEST(Run_WasmModule_CallAdd_rev) {
  static const int kModuleHeaderSize = 6;
  static const int kFunctionSize = 24;
  static const byte kCodeStartOffset0 =
      kModuleHeaderSize + 2 + kFunctionSize * 2;
  static const byte kCodeEndOffset0 = kCodeStartOffset0 + 6;
  static const byte kCodeStartOffset1 = kCodeEndOffset0;
  static const byte kCodeEndOffset1 = kCodeEndOffset0 + 7;
  static const byte data[] = {
      MODULE_HEADER(0, 2, 0),  // globals, functions, data segments
      // func#0 (main) ----------------------------------
      0, kAstInt32,                // signature: void -> int
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
      2, kAstInt32, kAstInt32, kAstInt32,  // signature: int,int -> int
      0, 0, 0, 0,                          // name offset
      kCodeStartOffset0, 0, 0, 0,          // code start offset
      kCodeEndOffset0, 0, 0, 0,            // code end offset
      0, 0,                                // local int32 count
      0, 0,                                // local int64 count
      0, 0,                                // local float32 count
      0, 0,                                // local float64 count
      0,                                   // exported
      0,                                   // external
      // body#0 -----------------------------------------
      kStmtReturn,       // --
      kExprInt32Add,     // --
      kExprGetLocal, 0,  // --
      kExprGetLocal, 1,  // --
      // body#1 -----------------------------------------
      kStmtReturn,           // --
      kExprCallFunction, 1,  // --
      kExprInt8Const, 77,    // --
      kExprInt8Const, 22     // --
  };

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(99, result);
}


TEST(Run_WasmModule_ReadLoadedDataSegment) {
  static const int kModuleHeaderSize = 6;
  static const int kFunctionSize = 24;
  static const int kDataSegmentSize = 13;
  static const byte kCodeStartOffset0 =
      kModuleHeaderSize + kFunctionSize * 1 + kDataSegmentSize;
  static const byte kCodeEndOffset0 = kCodeStartOffset0 + 5;
  static const byte kDataSegmentOffset0 = kCodeEndOffset0;
  static const byte kDataSegmentSize0 = 4;
  static const byte kDataSegmentDest0 = 12;
  static const byte data[] = {
      MODULE_HEADER(0, 1, 1),  // globals, functions, data segments
      // func#0 -----------------------------------------
      0, kAstInt32,                // signature: void -> int
      0, 0, 0, 0,                  // name offset
      kCodeStartOffset0, 0, 0, 0,  // code start offset
      kCodeEndOffset0, 0, 0, 0,    // code end offset
      0, 0,                        // local int32 count
      0, 0,                        // local int64 count
      0, 0,                        // local float32 count
      0, 0,                        // local float64 count
      1,                           // exported
      0,                           // external
      // segment#0 -------------------------------------------------
      kDataSegmentDest0, 0, 0, 0,    // dest addr
      kDataSegmentOffset0, 0, 0, 0,  // source offset
      kDataSegmentSize0, 0, 0, 0,    // source size
      1,                             // init
      // body#0 -----------------------------------------
      kStmtReturn,                                // --
      kExprInt32LoadMemL,                         // --
      WasmOpcodes::LoadStoreAccessOf(kMemInt32),  // --
      kExprInt8Const,                             // --
      kDataSegmentDest0,                          // --
      // data -----------------------------------------
      0xaa, 0xbb, 0xcc, 0xdd  // --
  };

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(0xddccbbaa, result);
}


TEST(Run_WasmModule_CheckMemoryIsZero) {
  static const int kCheckSize = 16 * 1024;
  static const int kModuleHeaderSize = 6;
  static const int kFunctionSize = 24;
  static const byte kCodeStartOffset0 = kModuleHeaderSize + kFunctionSize * 1;
  static const byte kCodeEndOffset0 = kCodeStartOffset0 + 31;
  static const byte data[] = {
      MODULE_HEADER(0, 1, 0),  // globals, functions, data segments
      // func#0 -----------------------------------------
      0, kAstInt32,                // signature: void -> int
      0, 0, 0, 0,                  // name offset
      kCodeStartOffset0, 0, 0, 0,  // code start offset
      kCodeEndOffset0, 0, 0, 0,    // code end offset
      1, 0,                        // local int32 count
      0, 0,                        // local int64 count
      0, 0,                        // local float32 count
      0, 0,                        // local float64 count
      1,                           // exported
      0,                           // external
      // body#0 -----------------------------------------
      WASM_WHILE(
          WASM_INT32_SLT(WASM_GET_LOCAL(0), WASM_INT32(kCheckSize)),
          WASM_IF_THEN(WASM_LOAD_MEM(kMemInt32, WASM_GET_LOCAL(0)),
                       WASM_RETURN(WASM_INT8(-1)), WASM_INC_LOCAL_BY(0, 4))),
      WASM_INT8(11),
  };

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(11, result);
}


TEST(Run_WasmModule_CallMain_recursive) {
  static const int kModuleHeaderSize = 6;
  static const int kFunctionSize = 24;
  static const byte kCodeStartOffset0 = kModuleHeaderSize + kFunctionSize;
  static const byte kCodeEndOffset0 = kCodeStartOffset0 + 33;
  static const byte data[] = {
      MODULE_HEADER(0, 1, 0),  // globals, functions, data segments
      // func#0 (main) ----------------------------------
      0, kAstInt32,                // signature: void -> int
      0, 0, 0, 0,                  // name offset
      kCodeStartOffset0, 0, 0, 0,  // code start offset
      kCodeEndOffset0, 0, 0, 0,    // code end offset
      1, 0,                        // local int32 count
      0, 0,                        // local int64 count
      0, 0,                        // local float32 count
      0, 0,                        // local float64 count
      1,                           // exported
      0,                           // external
      // body#0 -----------------------------------------
      WASM_BLOCK(
          2,                                                             //--
          WASM_SET_LOCAL(0, WASM_LOAD_MEM(kMemInt32, WASM_ZERO)),        // --
          WASM_IF_THEN(WASM_INT32_SLT(WASM_GET_LOCAL(0), WASM_INT8(5)),  // --
                       WASM_BLOCK(2,                                     // --
                                  WASM_STORE_MEM(kMemInt32, WASM_ZERO,   // --
                                                 WASM_INC_LOCAL(0)),     // --
                                  WASM_RETURN(WASM_CALL_FUNCTION0(0))),  // --
                       WASM_RETURN(WASM_INT8(55))))                      // --
  };

  CHECK_EQ(kCodeEndOffset0 - kCodeStartOffset0,
           arraysize(data) - kCodeStartOffset0);

  Isolate* isolate = CcTest::InitIsolateOnce();
  int32_t result =
      CompileAndRunWasmModule(isolate, data, data + arraysize(data));
  CHECK_EQ(55, result);
}
