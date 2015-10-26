// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/wasm/asm-wasm-builder.h"
#include "src/wasm/wasm-opcodes.h"

#include "src/ast.h"
#include "src/codegen.h"
#include "src/scopes.h"

namespace v8 {
namespace internal {
namespace wasm {

#define RECURSE(call)            \
  do {                           \
    DCHECK(!HasStackOverflow()); \
    call;                        \
    if (HasStackOverflow())      \
      return;                    \
  } while (false)

class AsmWasmBuilderImpl : public AstVisitor {
 public:
  AsmWasmBuilderImpl(Isolate* isolate, Zone* zone, FunctionLiteral* literal)
      : local_variables_(HashMap::PointersMatch,
                         ZoneHashMap::kDefaultHashMapCapacity,
                         ZoneAllocationPolicy(zone)),
        functions_(HashMap::PointersMatch,
                   ZoneHashMap::kDefaultHashMapCapacity,
                   ZoneAllocationPolicy(zone)),
        in_function_(false),
        is_set_op_(false),
        marking_exported(false),
        builder_(new (zone) WasmModuleBuilder(zone)),
        current_function_builder_(NULL),
        literal_(literal),
        isolate_(isolate),
        zone_(zone) {
    InitializeAstVisitor(isolate);
  }

  void Compile() { RECURSE(VisitFunctionLiteral(literal_)); }

  void VisitVariableDeclaration(VariableDeclaration* decl) {}

  void VisitFunctionDeclaration(FunctionDeclaration* decl) {
    DCHECK(!in_function_);
    DCHECK(current_function_builder_ == NULL);
    uint16_t index = LookupOrInsertFunction(decl->proxy()->var());
    current_function_builder_ = builder_->FunctionAt(index);
    in_function_ = true;
    RECURSE(Visit(decl->fun()));
    in_function_ = false;
    current_function_builder_ = NULL;
    local_variables_.Clear();
  }

  void VisitImportDeclaration(ImportDeclaration* decl) {}

  void VisitExportDeclaration(ExportDeclaration* decl) {}

  void VisitStatements(ZoneList<Statement*>* stmts) {
    for (int i = 0; i < stmts->length(); ++i) {
      Statement* stmt = stmts->at(i);
      RECURSE(Visit(stmt));
      if (stmt->IsJump())
        break;
    }
  }

  void VisitBlock(Block* stmt) { RECURSE(VisitStatements(stmt->statements())); }

  void VisitExpressionStatement(ExpressionStatement* stmt) {
    RECURSE(Visit(stmt->expression()));
  }

  void VisitEmptyStatement(EmptyStatement* stmt) {}

  void VisitEmptyParentheses(EmptyParentheses* paren) { UNREACHABLE(); }

  void VisitIfStatement(IfStatement* stmt) {
    RECURSE(Visit(stmt->condition()));
    RECURSE(Visit(stmt->then_statement()));
    RECURSE(Visit(stmt->else_statement()));
  }

  void VisitContinueStatement(ContinueStatement* stmt) {}

  void VisitBreakStatement(BreakStatement* stmt) {}

  void VisitReturnStatement(ReturnStatement* stmt) {
    if (in_function_) {
      current_function_builder_->AppendCode(kStmtReturn, false);
    } else {
      marking_exported = true;
    }
    RECURSE(Visit(stmt->expression()));
    if (!in_function_) {
      marking_exported = false;
    }
  }

  void VisitWithStatement(WithStatement* stmt) {
    RECURSE(stmt->expression());
    RECURSE(stmt->statement());
  }

  void VisitSwitchStatement(SwitchStatement* stmt) {
    RECURSE(Visit(stmt->tag()));

    ZoneList<CaseClause*>* clauses = stmt->cases();

    for (int i = 0; i < clauses->length(); ++i) {
      CaseClause* clause = clauses->at(i);
      if (!clause->is_default()) {
        Expression* label = clause->label();
        RECURSE(Visit(label));
      }
      ZoneList<Statement*>* stmts = clause->statements();
      RECURSE(VisitStatements(stmts));
    }
  }

  void VisitCaseClause(CaseClause* clause) { UNREACHABLE(); }

  void VisitDoWhileStatement(DoWhileStatement* stmt) {
    RECURSE(Visit(stmt->body()));
    RECURSE(Visit(stmt->cond()));
  }

  void VisitWhileStatement(WhileStatement* stmt) {
    RECURSE(Visit(stmt->cond()));
    RECURSE(Visit(stmt->body()));
  }

  void VisitForStatement(ForStatement* stmt) {
    if (stmt->init() != NULL) {
      RECURSE(Visit(stmt->init()));
    }
    if (stmt->cond() != NULL) {
      RECURSE(Visit(stmt->cond()));
    }
    if (stmt->next() != NULL) {
      RECURSE(Visit(stmt->next()));
    }
    RECURSE(Visit(stmt->body()));
  }

  void VisitForInStatement(ForInStatement* stmt) {
    RECURSE(Visit(stmt->enumerable()));
    RECURSE(Visit(stmt->body()));
  }

  void VisitForOfStatement(ForOfStatement* stmt) {
    RECURSE(Visit(stmt->iterable()));
    RECURSE(Visit(stmt->body()));
  }

  void VisitTryCatchStatement(TryCatchStatement* stmt) {
    RECURSE(Visit(stmt->try_block()));
    RECURSE(Visit(stmt->catch_block()));
  }

  void VisitTryFinallyStatement(TryFinallyStatement* stmt) {
    RECURSE(Visit(stmt->try_block()));
    RECURSE(Visit(stmt->finally_block()));
  }

  void VisitDebuggerStatement(DebuggerStatement* stmt) {}

  void VisitFunctionLiteral(FunctionLiteral* expr) {
    Scope* scope = expr->scope();
    // TODO: confirm where to get type
    if (in_function_) {
      current_function_builder_->ReturnType(kAstI32);
      for (int i = 0; i < expr->parameter_count(); i++) {
        LookupOrInsertLocal(scope->parameter(i), true);
      }
    }
    RECURSE(VisitDeclarations(scope->declarations()));
    RECURSE(VisitStatements(expr->body()));
  }

  void VisitNativeFunctionLiteral(NativeFunctionLiteral* expr) {}

  void VisitConditional(Conditional* expr) {
    RECURSE(Visit(expr->condition()));
    RECURSE(Visit(expr->then_expression()));
    RECURSE(Visit(expr->else_expression()));
  }

  void VisitVariableProxy(VariableProxy* expr) {
    if (in_function_) {
      Variable* var = expr->var();
      if (var->is_function()) {
        std::vector<uint8_t> index =
            UnsignedLEB128From(LookupOrInsertFunction(var));
        current_function_builder_->AddBody(index.data(),
                                           static_cast<uint32_t>(index.size()));
      } else {
        if (!is_set_op_) {
          current_function_builder_->AppendCode(kExprGetLocal, false);
        }
        std::vector<uint8_t> index =
            UnsignedLEB128From(LookupOrInsertLocal(var, false));
        uint32_t pos_of_index[1] = {0};
        current_function_builder_->AddBody(
            index.data(), static_cast<uint32_t>(index.size()), pos_of_index, 1);
      }
    } else if (marking_exported) {
      Variable* var = expr->var();
      if (var->is_function()) {
        uint16_t index = LookupOrInsertFunction(var);
        builder_->FunctionAt(index)->Exported(1);
      }
    }
  }

  void VisitLiteral(Literal* expr) {
    if (in_function_) {
      if (expr->raw_value()->IsNumber()) {
        // TODO: deal with type
        int val = static_cast<int>(expr->raw_value()->AsNumber());
        current_function_builder_->AppendCode(kExprI32Const, false);
        current_function_builder_->AppendCode(static_cast<byte>(val), false);
        current_function_builder_->AppendCode(static_cast<byte>(val >> 8),
                                              false);
        current_function_builder_->AppendCode(static_cast<byte>(val >> 16),
                                              false);
        current_function_builder_->AppendCode(static_cast<byte>(val >> 24),
                                              false);
      }
    }
  }

  void VisitRegExpLiteral(RegExpLiteral* expr) {}

  void VisitObjectLiteral(ObjectLiteral* expr) {
    ZoneList<ObjectLiteralProperty*>* props = expr->properties();
    for (int i = 0; i < props->length(); ++i) {
      ObjectLiteralProperty* prop = props->at(i);
      RECURSE(Visit(prop->value()));
    }
  }

  void VisitArrayLiteral(ArrayLiteral* expr) {
    ZoneList<Expression*>* values = expr->values();
    for (int i = 0; i < values->length(); ++i) {
      Expression* value = values->at(i);
      RECURSE(Visit(value));
    }
  }

  void VisitAssignment(Assignment* expr) {
    Property* property = expr->target()->AsProperty();
    LhsKind assign_type = Property::GetAssignType(property);

    // Evaluate LHS expression.
    switch (assign_type) {
      case VARIABLE:
        DCHECK(in_function_);
        current_function_builder_->AppendCode(kExprSetLocal, false);
        is_set_op_ = true;
        RECURSE(Visit(expr->target()));
        is_set_op_ = false;
        RECURSE(Visit(expr->value()));
        break;
      default:
        UNREACHABLE();
    }
  }

  void VisitYield(Yield* expr) {
    RECURSE(Visit(expr->generator_object()));
    RECURSE(Visit(expr->expression()));
  }

  void VisitThrow(Throw* expr) { RECURSE(Visit(expr->exception())); }

  void VisitProperty(Property* expr) {
    RECURSE(Visit(expr->obj()));
    RECURSE(Visit(expr->key()));
  }

  void VisitCall(Call* expr) {
    Call::CallType call_type = expr->GetCallType(isolate_);
    switch (call_type) {
      case Call::OTHER_CALL: {
        DCHECK(in_function_);
        current_function_builder_->AppendCode(kExprCallFunction, false);
        RECURSE(Visit(expr->expression()));
        ZoneList<Expression*>* args = expr->arguments();
        for (int i = 0; i < args->length(); ++i) {
          Expression* arg = args->at(i);
          RECURSE(Visit(arg));
        }
        break;
      }
      default:
        UNREACHABLE();
    }
  }

  void VisitCallNew(CallNew* expr) { UNREACHABLE(); }

  void VisitCallRuntime(CallRuntime* expr) { UNREACHABLE(); }

  void VisitUnaryOperation(UnaryOperation* expr) {
    RECURSE(Visit(expr->expression()));
  }

  void VisitCountOperation(CountOperation* expr) {
    RECURSE(Visit(expr->expression()));
  }

  void VisitBinaryOperation(BinaryOperation* expr) {
    switch (expr->op()) {
      case Token::BIT_OR:
        current_function_builder_->AppendCode(kExprI32Ior, false);
        break;
      case Token::ADD:
        current_function_builder_->AppendCode(kExprI32Add, false);
        break;
      default:
        DCHECK(false);
        break;
    }
    RECURSE(Visit(expr->left()));
    RECURSE(Visit(expr->right()));
  }

  void VisitCompareOperation(CompareOperation* expr) {
    RECURSE(Visit(expr->left()));
    RECURSE(Visit(expr->right()));
  }

  void VisitThisFunction(ThisFunction* expr) {}

  void VisitDeclarations(ZoneList<Declaration*>* decls) {
    for (int i = 0; i < decls->length(); ++i) {
      Declaration* decl = decls->at(i);
      RECURSE(Visit(decl));
    }
  }

  void VisitClassLiteral(ClassLiteral* expr) {}

  void VisitSpread(Spread* expr) {}

  void VisitSuperPropertyReference(SuperPropertyReference* expr) {}

  void VisitSuperCallReference(SuperCallReference* expr) {}

  void VisitSloppyBlockFunctionStatement(SloppyBlockFunctionStatement* expr) {}

  void VisitDoExpression(DoExpression* expr) {}

  struct IndexContainer : public ZoneObject {
    uint16_t index;
  };

  uint16_t LookupOrInsertLocal(Variable* v, bool is_param) {
    DCHECK(current_function_builder_ != NULL);
    ZoneHashMap::Entry* entry =
        local_variables_.Lookup(v, ComputePointerHash(v));
    if (entry == NULL) {
      uint16_t index;
      if (is_param) {
        index = current_function_builder_->AddParam(TypeOf(v));
      } else {
        index = current_function_builder_->AddLocal(TypeOf(v));
      }
      IndexContainer* container = new (zone()) IndexContainer();
      container->index = index;
      entry = local_variables_.LookupOrInsert(v, ComputePointerHash(v),
                                              ZoneAllocationPolicy(zone()));
      entry->value = container;
    }
    return (reinterpret_cast<IndexContainer*>(entry->value))->index;
  }

  uint16_t LookupOrInsertFunction(Variable* v) {
    DCHECK(builder_ != NULL);
    ZoneHashMap::Entry* entry = functions_.Lookup(v, ComputePointerHash(v));
    if (entry == NULL) {
      uint16_t index = builder_->AddFunction();
      IndexContainer* container = new (zone()) IndexContainer();
      container->index = index;
      entry = functions_.LookupOrInsert(v, ComputePointerHash(v),
                                        ZoneAllocationPolicy(zone()));
      entry->value = container;
    }
    return (reinterpret_cast<IndexContainer*>(entry->value))->index;
  }

  uint8_t TypeOf(Variable* v) { return kAstI32; }

  Zone* zone() { return zone_; }

  ZoneHashMap local_variables_;
  ZoneHashMap functions_;
  bool in_function_;
  bool is_set_op_;
  bool marking_exported;
  WasmModuleBuilder* builder_;
  WasmFunctionBuilder* current_function_builder_;
  FunctionLiteral* literal_;
  Isolate* isolate_;
  Zone* zone_;

  DEFINE_AST_VISITOR_SUBCLASS_MEMBERS();
  DISALLOW_COPY_AND_ASSIGN(AsmWasmBuilderImpl);
};

AsmWasmBuilder::AsmWasmBuilder(Isolate* isolate,
                               Zone* zone,
                               FunctionLiteral* literal)
    : isolate_(isolate), zone_(zone), literal_(literal) {
}

/*TODO: probably should take zone (to write wasm to) as input so that zone in
  constructor may be thrown away once wasm module is written */
WasmModuleIndex* AsmWasmBuilder::Run() {
  AsmWasmBuilderImpl impl(isolate_, zone_, literal_);
  impl.Compile();
  WasmModuleWriter* writer = impl.builder_->Build(zone_);
  return writer->WriteTo(zone_);
}

}  // namespace wasm
}  // namespace internal
}  // namespace v8
