// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_TF_BUILDER_H_
#define V8_WASM_TF_BUILDER_H_

#include "src/zone.h"

#include "src/wasm/wasm-opcodes.h"

namespace v8 {
namespace internal {

namespace compiler {
// Forward declarations for some compiler data structures.
class Node;
class JSGraph;
}

namespace wasm {
// Forward declarations for some WASM data structures.
struct ModuleEnv;
struct WasmFunction;
class ErrorThrower;

// Expose {Node} and {Graph} opaquely as {wasm::TFNode} and {wasm::TFGraph}.
typedef compiler::Node TFNode;
typedef compiler::JSGraph TFGraph;
}

namespace compiler {
// Compiles a single function, producing a code object.
Handle<Code> CompileWasmFunction(wasm::ErrorThrower& thrower, Isolate* isolate,
                                 wasm::ModuleEnv* module_env,
                                 const wasm::WasmFunction& function, int index);

// Wraps a JS function, producing a code object that can be called from WASM.
Handle<Code> CompileWasmToJSWrapper(Isolate* isolate, wasm::ModuleEnv* module,
                                    Handle<JSFunction> function,
                                    uint32_t index);

// Wraps a given wasm code object, producing a JSFunction that can be called
// from JavaScript.
Handle<JSFunction> CompileJSToWasmWrapper(Isolate* isolate,
                                          wasm::ModuleEnv* module,
                                          Handle<String> name,
                                          Handle<Code> wasm_code,
                                          uint32_t index);

// Abstracts details of building TurboFan graph nodes for WASM to separate
// the WASM decoder from the internal details of TurboFan.
class WasmTrapHelper;
class WasmGraphBuilder {
 public:
  WasmGraphBuilder(Zone* z, JSGraph* g);

  Node** Buffer(size_t count) {
    if (count > cur_bufsize) {
      size_t new_size = count + cur_bufsize + 5;
      cur_buffer =
          reinterpret_cast<Node**>(zone->New(new_size * sizeof(Node*)));
      cur_bufsize = new_size;
    }
    return cur_buffer;
  }

  //-----------------------------------------------------------------------
  // Operations independent of {control} or {effect}.
  //-----------------------------------------------------------------------
  Node* Error();
  Node* Start(unsigned params);
  Node* Param(unsigned index, wasm::LocalType type);
  Node* Loop(Node* entry);
  Node* Terminate(Node* effect, Node* control);
  Node* Merge(unsigned count, Node** controls);
  Node* Phi(wasm::LocalType type, unsigned count, Node** vals, Node* control);
  Node* EffectPhi(unsigned count, Node** effects, Node* control);
  Node* Int32Constant(int32_t value);
  Node* Int64Constant(int64_t value);
  Node* Float32Constant(float value);
  Node* Float64Constant(double value);
  Node* Constant(Handle<Object> value);
  Node* Binop(wasm::WasmOpcode opcode, Node* left, Node* right);
  Node* Unop(wasm::WasmOpcode opcode, Node* input);
  unsigned InputCount(Node* node);
  bool IsPhiWithMerge(Node* phi, Node* merge);
  void AppendToMerge(Node* merge, Node* from);
  void AppendToPhi(Node* merge, Node* phi, Node* from);

  //-----------------------------------------------------------------------
  // Operations that read and/or write {control} and {effect}.
  //-----------------------------------------------------------------------
  Node* Branch(Node* cond, Node** true_node, Node** false_node);
  Node* Switch(unsigned count, Node* key);
  Node* IfValue(int32_t value, Node* sw);
  Node* IfDefault(Node* sw);
  Node* Return(unsigned count, Node** vals);
  Node* ReturnVoid();
  Node* Unreachable();

  Node* CallDirect(uint32_t index, Node** args);
  Node* CallIndirect(uint32_t index, Node** args);
  void BuildJSToWasmWrapper(Handle<Code> wasm_code, wasm::FunctionSig* sig);
  void BuildWasmToJSWrapper(Handle<JSFunction> function,
                            wasm::FunctionSig* sig);
  Node* ToJS(Node* node, Node* context, wasm::LocalType type);
  Node* FromJS(Node* node, Node* context, wasm::LocalType type);
  Node* Invert(Node* node);
  Node* FunctionTable();

  //-----------------------------------------------------------------------
  // Operations that concern the linear memory.
  //-----------------------------------------------------------------------
  Node* MemSize(uint32_t offset);
  Node* LoadGlobal(uint32_t index);
  Node* StoreGlobal(uint32_t index, Node* val);
  Node* LoadMem(wasm::LocalType type, MachineType memtype, Node* index,
                uint32_t offset);
  Node* StoreMem(MachineType type, Node* index, uint32_t offset, Node* val);

  static void PrintDebugName(Node* node);

  Node* Control() { return *control; }
  Node* Effect() { return *effect; }

  void set_module(wasm::ModuleEnv* env) { this->module = env; }

  void set_control_ptr(Node** control) { this->control = control; }

  void set_effect_ptr(Node** effect) { this->effect = effect; }

 private:
  static const int kDefaultBufferSize = 16;
  friend class WasmTrapHelper;

  Zone* zone;
  JSGraph* graph;
  wasm::ModuleEnv* module;
  Node* mem_buffer;
  Node* mem_size;
  Node* function_table;
  Node** control;
  Node** effect;
  Node** cur_buffer;
  size_t cur_bufsize;
  Node* def_buffer[kDefaultBufferSize];

  WasmTrapHelper* trap;

  // Internal helper methods.
  Node* String(const char* string);
  Node* MemBuffer(uint32_t offset);
  void BoundsCheckMem(MachineType memtype, Node* index, uint32_t offset);

  Node* BuildWasmCall(wasm::FunctionSig* sig, Node** args);
  Node* BuildF32CopySign(Node* left, Node* right);
  Node* BuildF64CopySign(Node* left, Node* right);
  Node* BuildI32Ctz(Node* input);
  Node* BuildI32Popcnt(Node* input);
  Node* BuildI64Ctz(Node* input);
  Node* BuildI64Popcnt(Node* input);

  Node** Realloc(Node** buffer, size_t count) {
    Node** buf = Buffer(count);
    if (buf != buffer) memcpy(buf, buffer, count * sizeof(Node*));
    return buf;
  }
};
}
}
}  // namespace v8::internal::wasm

#endif  // V8_WASM_TF_BUILDER_H_
