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

namespace v8 {
namespace internal {
namespace wasm {

// A Visitor over a CompilationInfo's AST that generates WASM.

class AsmWasmBuilder : public AstVisitor {
 public:
  explicit AsmWasmBuilder(CompilationInfo* info);
  void Run();

 private:
  void VisitDeclarations(ZoneList<Declaration*>* d) override;
  void VisitStatements(ZoneList<Statement*>* s) override;

  DEFINE_AST_VISITOR_SUBCLASS_MEMBERS();

#define DECLARE_VISIT(type) virtual void Visit##type(type* node) override;
  AST_NODE_LIST(DECLARE_VISIT)
#undef DECLARE_VISIT

  CompilationInfo* compilation_info_;

  DISALLOW_COPY_AND_ASSIGN(AsmWasmBuilder);
};
}
}
}

#endif  // V8_WASM_ASM_WASM_BUILDER_H_
