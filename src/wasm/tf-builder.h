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

class TFTrapHelper;

// Abstracts details of building TurboFan graph nodes for WASM to separate
// the WASM decoder from the internal details of TurboFan.
class TFBuilder {
 public:
  TFBuilder(Zone* z, TFGraph* g);

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
  TFNode* Branch(TFNode* cond, TFNode** true_node, TFNode** false_node);
  TFNode* Switch(unsigned count, TFNode* key);
  TFNode* IfValue(int32_t value, TFNode* sw);
  TFNode* IfDefault(TFNode* sw);
  TFNode* Return(unsigned count, TFNode** vals);
  TFNode* ReturnVoid();
  TFNode* Unreachable();

  TFNode* CallDirect(uint32_t index, TFNode** args);
  TFNode* CallIndirect(uint32_t index, TFNode** args);
  void BuildJSToWasmWrapper(Handle<Code> wasm_code, FunctionSig* sig);
  void BuildWasmToJSWrapper(Handle<JSFunction> function, FunctionSig* sig);
  TFNode* ToJS(TFNode* node, TFNode* context, LocalType type);
  TFNode* FromJS(TFNode* node, TFNode* context, LocalType type);
  TFNode* Invert(TFNode* node);
  TFNode* FunctionTable();

  //-----------------------------------------------------------------------
  // Operations that concern the linear memory.
  //-----------------------------------------------------------------------
  TFNode* MemSize(uint32_t offset);
  TFNode* LoadGlobal(uint32_t index);
  TFNode* StoreGlobal(uint32_t index, TFNode* val);
  TFNode* LoadMem(LocalType type, MemType memtype, TFNode* index,
                  uint32_t offset);
  TFNode* StoreMem(MemType type, TFNode* index, uint32_t offset, TFNode* val);

  static void PrintDebugName(TFNode* node);

  TFNode* Control() { return *control; }
  TFNode* Effect() { return *effect; }

  void set_module(ModuleEnv* env) { this->module = env; }

  void set_control_ptr(TFNode** control) { this->control = control; }

  void set_effect_ptr(TFNode** effect) { this->effect = effect; }

 private:
  static const int kDefaultBufferSize = 16;
  friend class TFTrapHelper;

  Zone* zone;
  TFGraph* graph;
  ModuleEnv* module;
  TFNode* mem_buffer;
  TFNode* mem_size;
  TFNode* function_table;
  TFNode** control;
  TFNode** effect;
  TFNode** cur_buffer;
  size_t cur_bufsize;
  TFNode* def_buffer[kDefaultBufferSize];

  TFTrapHelper* trap;

  // Internal helper methods.
  TFNode* String(const char* string);
  TFNode* MemBuffer(uint32_t offset);
  void BoundsCheckMem(MemType memtype, TFNode* index, uint32_t offset);

  TFNode* BuildWasmCall(FunctionSig* sig, TFNode** args);
  TFNode* BuildF32CopySign(TFNode* left, TFNode* right);
  TFNode* BuildF64CopySign(TFNode* left, TFNode* right);
  TFNode* BuildI32Ctz(TFNode* input);
  TFNode* BuildI32Popcnt(TFNode* input);
  TFNode* BuildI64Ctz(TFNode* input);
  TFNode* BuildI64Popcnt(TFNode* input);

  TFNode** Realloc(TFNode** buffer, size_t count) {
    TFNode** buf = Buffer(count);
    if (buf != buffer) memcpy(buf, buffer, count * sizeof(TFNode*));
    return buf;
  }
};
}
}
}  // namespace v8::internal::wasm

#endif  // V8_WASM_TF_BUILDER_H_
