// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_TF_BUILDER_H_
#define V8_WASM_TF_BUILDER_H_

#include "src/zone.h"

#include "src/wasm/wasm-opcodes.h"

namespace v8 {
namespace internal {

namespace compiler {  // external declarations from compiler.
class Node;
class JSGraph;
}

namespace wasm {

typedef compiler::Node TFNode;
typedef compiler::JSGraph TFGraph;

struct ModuleEnv;

// Abstracts details of building TurboFan graph nodes, making the decoder
// independent of the exact IR details.
struct TFBuilder {
  static const int kDefaultBufferSize = 16;

  Zone* zone;
  TFGraph* graph;
  ModuleEnv* module;
  TFNode* mem_buffer;
  TFNode* mem_size;
  TFNode** control;
  TFNode** effect;
  TFNode** cur_buffer;
  size_t cur_bufsize;
  TFNode* def_buffer[kDefaultBufferSize];

  TFBuilder(Zone* z, TFGraph* g)
      : zone(z),
        graph(g),
        module(nullptr),
        mem_buffer(nullptr),
        mem_size(nullptr),
        control(nullptr),
        effect(nullptr),
        cur_buffer(def_buffer),
        cur_bufsize(kDefaultBufferSize) {}

  TFNode** Buffer(size_t count) {
    if (count > cur_bufsize) {
      size_t new_size = count + cur_bufsize + 5;
      cur_buffer =
          reinterpret_cast<TFNode**>(zone->New(new_size * sizeof(TFNode*)));
      cur_bufsize = new_size;
    }
    return cur_buffer;
  }

  //-----------------------------------------------------------------------
  // Operations independent of {control} or {effect}.
  //-----------------------------------------------------------------------
  TFNode* Error();
  TFNode* Start(unsigned params);
  TFNode* Param(unsigned index, LocalType type);
  TFNode* Loop(TFNode* entry);
  TFNode* Terminate(TFNode* effect, TFNode* control);
  TFNode* Merge(unsigned count, TFNode** controls);
  TFNode* Phi(LocalType type, unsigned count, TFNode** vals, TFNode* control);
  TFNode* EffectPhi(unsigned count, TFNode** effects, TFNode* control);
  TFNode* Int32Constant(int32_t value);
  TFNode* Int64Constant(int64_t value);
  TFNode* Float32Constant(float value);
  TFNode* Float64Constant(double value);
  TFNode* Constant(Handle<Object> value);
  TFNode* Binop(WasmOpcode opcode, TFNode* left, TFNode* right);
  TFNode* Unop(WasmOpcode opcode, TFNode* input);
  unsigned InputCount(TFNode* node);
  bool IsPhiWithMerge(TFNode* phi, TFNode* merge);
  void AppendToMerge(TFNode* merge, TFNode* from);
  void AppendToPhi(TFNode* merge, TFNode* phi, TFNode* from);

  //-----------------------------------------------------------------------
  // Operations that read and/or write {control} and {effect}.
  //-----------------------------------------------------------------------
  void Branch(TFNode* cond, TFNode** true_node, TFNode** false_node);
  void Return(unsigned count, TFNode** vals);
  void ReturnVoid();

  TFNode* FunctionTableLookup(unsigned index, TFNode* offset) {
    return nullptr;
  }

  TFNode* CallDirect(uint32_t index, TFNode** args);
  TFNode* CallIndirect(uint32_t table_index, TFNode** args);

  //-----------------------------------------------------------------------
  // Operations that access the mem.
  //-----------------------------------------------------------------------
  TFNode* MemBuffer();
  TFNode* MemSize();
  TFNode* LoadGlobal(uint32_t index);
  TFNode* StoreGlobal(uint32_t index, TFNode* val);
  TFNode* LoadMem(MemType type, TFNode* index);
  TFNode* StoreMem(MemType type, TFNode* index, TFNode* val);

  static void PrintDebugName(TFNode* node);
};
}
}
}  // namespace v8::internal::wasm


#endif  // V8_WASM_TF_BUILDER_H_
