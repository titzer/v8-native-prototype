// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WEBASM_TF_BUILDER_H_
#define V8_WEBASM_TF_BUILDER_H_

#include "src/zone.h"

namespace v8 {
namespace internal {

namespace compiler {  // external declarations from compiler.
class Node;
class JSGraph;
}

namespace webasm {

typedef compiler::Node TFNode;
typedef compiler::JSGraph TFGraph;

// Abstracts details of building TurboFan graph nodes, making the decoder
// independent of the exact IR details.
struct TFBuilder {
  static const int kDefaultBufferSize = 16;

  Zone* zone;
  TFGraph* graph;
  TFNode** control;
  TFNode** effect;
  TFNode** cur_buffer;
  size_t cur_bufsize;
  TFNode* def_buffer[kDefaultBufferSize];

  TFBuilder(Zone* z, TFGraph* g)
      : zone(z),
        graph(g),
        control(nullptr),
        effect(nullptr),
        cur_buffer(def_buffer),
        cur_bufsize(kDefaultBufferSize) {}

  TFNode** Buffer(unsigned count) {
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
  TFNode* Param(unsigned index, AstType type);
  TFNode* Loop(TFNode* entry);
  TFNode* Merge(unsigned count, TFNode** controls);
  TFNode* Phi(AstType type, unsigned count, TFNode** vals, TFNode* control);
  TFNode* EffectPhi(unsigned count, TFNode** effects, TFNode* control);
  TFNode* Int32Constant(int value);
  TFNode* Float32Constant(float value);
  TFNode* Float64Constant(double value);
  TFNode* Binop(WebAsmOpcode opcode, TFNode* left, TFNode* right);
  TFNode* Unop(WebAsmOpcode opcode, TFNode* left);
  unsigned InputCount(TFNode* node);
  bool IsPhiWithMerge(TFNode* phi, TFNode* merge);
  void AppendToMerge(TFNode* merge, TFNode* from);
  void AppendToPhi(TFNode* merge, TFNode* phi, TFNode* from);

  //-----------------------------------------------------------------------
  // Operations that read and/or write {control} and {effect}.
  //-----------------------------------------------------------------------
  void Branch(TFNode* cond, TFNode** true_node, TFNode** false_node);
  void Return(unsigned count, TFNode** vals);

  TFNode* FunctionConstant(unsigned index) { return nullptr; }
  TFNode* FunctionTableLookup(unsigned index, TFNode* offset) {
    return nullptr;
  }

  TFNode* Call(unsigned count, TFNode** vals) { return nullptr; }
  TFNode* GetGlobal(unsigned index) { return nullptr; }
  TFNode* SetGlobal(unsigned, TFNode* val) { return nullptr; }
  TFNode* GetHeap(MemType type, TFNode* index) { return nullptr; }
  TFNode* SetHeap(MemType type, TFNode* index, TFNode* val) { return nullptr; }
};
}
}
}  // namespace v8::internal::webasm


#endif  // V8_WEBASM_TF_BUILDER_H_
