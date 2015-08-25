// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_WRAPPER_H_
#define V8_WASM_WRAPPER_H_

#include "src/wasm/wasm-module.h"
#include "src/handles.h"

namespace v8 {
namespace internal {
namespace wasm {

// Wraps a JS function, producing a code object that can be called from WASM.
Handle<Code> CompileWasmToJSWrapper(Isolate* isolate, ModuleEnv* module,
                                    Handle<JSFunction> function,
                                    uint32_t index);

// Wraps a given wasm code object, producing a JSFunction that can be called
// from JavaScript.
Handle<JSFunction> CompileJSToWasmWrapper(Isolate* isolate, ModuleEnv* module,
                                          Handle<String> name,
                                          Handle<Code> wasm_code,
                                          uint32_t index);
}
}
}

#endif  // V8_WASM_WRAPPER_H_
