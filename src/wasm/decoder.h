// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_DECODER_H_
#define V8_WASM_DECODER_H_

#include "src/signature.h"
#include "src/zone-containers.h"

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

typedef Signature<LocalType> FunctionSig;

// Interface the module environment during decoding, including information about
// the global variables and the function tables.
struct ModuleEnv {
  uintptr_t mem_start;
  uintptr_t mem_end;

  ZoneVector<FunctionSig*>* function_sigs;
  ZoneVector<FunctionSig*>* function_table_sigs;
  ZoneVector<MemType>* global_types;

  bool IsValidGlobal(unsigned index) { return index < global_types->size(); }
  MemType GetGlobalType(unsigned index) { return global_types->at(index); }
  FunctionSig* GetFunctionSignature(unsigned index) {
    return function_sigs->at(index);
  }
  FunctionSig* GetFunctionTableSignature(unsigned index) {
    return function_table_sigs->at(index);
  }
};

// Interface the function environment during decoding, include the signature
// and number of locals.
struct FunctionEnv {
  ModuleEnv* module;             // module environment
  FunctionSig* sig;              // signature of this function
  unsigned local_int32_count;    // number of int32 locals
  unsigned local_int64_count;    // number of int64 locals
  unsigned local_float32_count;  // number of float32 locals
  unsigned local_float64_count;  // number of float64 locals
  unsigned total_locals;         // sum of parameters and all locals

  bool IsValidLocal(unsigned index) { return index < total_locals; }
  unsigned GetLocalCount() { return total_locals; }
  LocalType GetLocalType(unsigned index) {
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

  void AddLocals(LocalType type, unsigned count) {
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
};

struct Tree;

// Error codes for programmatic checking of the decoder's verification.
enum ErrorCode {
  kSuccess,
  kError,                 // TODO(titzer): remove me
  kOutOfMemory,           // decoder ran out of memory
  kEndOfCode,             // end of code reached prematurely
  kInvalidOpcode,         // found invalid opcode
  kUnreachableCode,       // found unreachable code
  kImproperContinue,      // improperly nested continue
  kImproperBreak,         // improperly nested break
  kReturnCount,           // return count mismatch
  kTypeError,             // type mismatch
  kInvalidLocalIndex,     // invalid local
  kInvalidGlobalIndex,    // invalid global
  kInvalidFunctionIndex,  // invalid function
  kInvalidMemType         // invalid memory type
};

// The overall result of decoding.
struct Result {
  Tree* tree;
  ErrorCode error_code;
  const byte* pc;
  const byte* error_pc;
  const byte* error_pt;
  const char* error_msg;
};

std::ostream& operator<<(std::ostream& os, const Tree& tree);
std::ostream& operator<<(std::ostream& os, const Result& result);
std::ostream& operator<<(std::ostream& os, const ErrorCode& error_code);

Result VerifyWasmCode(FunctionEnv* env, const byte* start, const byte* end);
Result BuildTFGraph(TFGraph* graph, FunctionEnv* env, const byte* start,
                    const byte* end);
}
}
}


#endif  // V8_WASM_DECODER_H_
