// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_ASM_WASM_BUILDER_H_
#define V8_WASM_ASM_WASM_BUILDER_H_

#include "src/allocation.h"
#include "src/ast.h"
#include "src/effects.h"
#include "src/scopes.h"
#include "src/type-info.h"
#include "src/types.h"
#include "src/zone.h"
#include "third_party/wasm/src/wasm/encoder.h"

namespace v8 {
namespace internal {
namespace wasm {

class AsmWasmBuilder {
 public:
  explicit AsmWasmBuilder(CompilationInfo* info);
  WasmModuleIndex* Run();

 private:
  CompilationInfo* info_;
};

}
}
}

#endif  // V8_WASM_ASM_WASM_BUILDER_H_