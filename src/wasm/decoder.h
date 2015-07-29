// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_DECODER_H_
#define V8_WASM_DECODER_H_

#include "src/signature.h"
#include "src/zone-containers.h"

#include "src/base/smart-pointers.h"

#include "src/wasm/wasm-opcodes.h"
#include "src/wasm/wasm-result.h"

namespace v8 {
namespace internal {

namespace compiler {  // external declarations from compiler.
class Node;
class JSGraph;
}

namespace wasm {

typedef compiler::Node TFNode;
typedef compiler::JSGraph TFGraph;
typedef Signature<LocalType> FunctionSig;

struct ModuleEnv;  // forward declaration of module interface.

// Interface the function environment during decoding, include the signature
// and number of locals.
struct FunctionEnv {
  ModuleEnv* module;             // module environment
  FunctionSig* sig;              // signature of this function
  uint32_t local_int32_count;    // number of int32 locals
  uint32_t local_int64_count;    // number of int64 locals
  uint32_t local_float32_count;  // number of float32 locals
  uint32_t local_float64_count;  // number of float64 locals
  uint32_t total_locals;         // sum of parameters and all locals

  bool IsValidLocal(uint32_t index) { return index < total_locals; }
  uint32_t GetLocalCount() { return total_locals; }
  LocalType GetLocalType(uint32_t index) {
    if (index < sig->parameter_count()) return sig->GetParam(index);
    index -= sig->parameter_count();
    if (index < local_int32_count) return kAstInt32;
    index -= local_int32_count;
    if (index < local_int64_count) return kAstInt64;
    index -= local_int64_count;
    if (index < local_float32_count) return kAstFloat32;
    index -= local_float32_count;
    if (index < local_float64_count) return kAstFloat64;
    return kAstStmt;
  }

  void AddLocals(LocalType type, uint32_t count) {
    switch (type) {
      case kAstInt32:
        local_int32_count += count;
        break;
      case kAstInt64:
        local_int64_count += count;
        break;
      case kAstFloat32:
        local_float32_count += count;
        break;
      case kAstFloat64:
        local_float64_count += count;
        break;
      default:
        UNREACHABLE();
    }
    total_locals += count;
    DCHECK(total_locals ==
           (sig->parameter_count() + local_int32_count + local_int64_count +
            local_float32_count + local_float64_count));
  }

  void SumLocals() {
    total_locals = static_cast<uint32_t>(sig->parameter_count()) +
                   local_int32_count + local_int64_count + local_float32_count +
                   local_float64_count;
  }
};

struct Tree;
typedef Result<Tree*> TreeResult;

std::ostream& operator<<(std::ostream& os, const Tree& tree);

TreeResult VerifyWasmCode(FunctionEnv* env, const byte* start, const byte* end);
TreeResult BuildTFGraph(TFGraph* graph, FunctionEnv* env, const byte* start,
                        const byte* end);
}
}
}


#endif  // V8_WASM_DECODER_H_
