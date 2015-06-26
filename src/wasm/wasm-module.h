// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_MODULE_H_
#define V8_WASM_MODULE_H_

#include "wasm-opcodes.h"

namespace v8 {
namespace internal {
namespace wasm {

// Static representation of a wasm function.
struct WasmFunction {
  FunctionSig* sig;      // signature of the function.
  uint32_t name_offset;  // offset in the module bytes of the name, if any.
  uint32_t code_start_offset;    // offset in the module bytes of code start.
  uint32_t code_end_offset;      // offset in the module bytes of code end.
  uint16_t local_int32_count;    // number of int32 local variables.
  uint16_t local_float32_count;  // number of float32 local variables.
  uint16_t local_float64_count;  // number of float64 local variables.
  bool exported;                 // true if this function is exported.
  bool external;  // true if this function is externally supplied.
};

// Static representation of a wasm global variable.
struct WasmGlobal {
  uint32_t name_offset;  // offset in the module bytes of the name, if any.
  MemType type;          // type of the global.
  uint32_t offset;       // offset from beginning of globals area.
  bool exported;         // true if this global is exported.
};

// Static representation of a wasm data segment.
struct WasmDataSegment {
  uint32_t dest_addr;      // destination memory address of the data.
  uint32_t source_offset;  // start offset in the module bytes.
  uint32_t source_size;    // end offset in the module bytes.
  bool init;               // true if loaded upon instantiation.
};

// Static representation of a module.
struct WasmModule {
  static const uint8_t kMinMemSize = 12;  // Minimum memory size = 4kb
  static const uint8_t kMaxMemSize = 30;  // Maximum memory size = 1gb

  Isolate* shared_isolate;   // isolate for storing shared code.
  const byte* module_start;  // starting address for the module bytes.
  const byte* module_end;    // end address for the module bytes.
  uint8_t mem_size_log2;     // size of the memory (log base 2).
  bool mem_export;           // true if the memory is exported.
  bool mem_external;         // true if the memory is external.
  std::vector<WasmFunction>* functions;         // functions in this module.
  std::vector<WasmGlobal>* globals;             // globals in this module.
  std::vector<WasmDataSegment>* data_segments;  // data segments in this module.

  // Get a pointer to a string stored in the module bytes.
  const char* GetName(uint32_t offset) {
    DCHECK(BoundsCheck(offset, offset + 1));
    return reinterpret_cast<const char*>(module_start + offset);
  }

  // Checks the given offset range is contained within the module bytes.
  bool BoundsCheck(uint32_t start, uint32_t end) {
    size_t size = module_end - module_start;
    return start < size && end < size;
  }

  // Creates a new instantiation of the module in the given isolate.
  MaybeHandle<JSObject> Instantiate(Isolate* isolate);

  Handle<Code> Compile(int func_index, const WasmFunction& function);
};

WasmModule* DecodeWasmModule(Isolate* isolate, const byte* module_start,
                             const byte* module_end);
}
}
}

#endif  // V8_WASM_MODULE_H_
