// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/wasm/asm-wasm-builder.h"

#include "src/ast.h"
#include "src/codegen.h"
#include "src/scopes.h"

namespace v8 {
namespace internal {
namespace wasm {


#define RECURSE(call)               \
  do {                              \
    DCHECK(!HasStackOverflow());    \
    call;                           \
    if (HasStackOverflow()) return; \
  } while (false)


AsmWasmBuilder::AsmWasmBuilder(CompilationInfo* info)
    : compilation_info_(info) {
  InitializeAstVisitor(info->isolate(), info->zone());
}


void AsmWasmBuilder::Run() {
  RECURSE(VisitFunctionLiteral(compilation_info_->literal()));
}


void AsmWasmBuilder::VisitVariableDeclaration(VariableDeclaration* decl) {}


void AsmWasmBuilder::VisitFunctionDeclaration(FunctionDeclaration* decl) {
  RECURSE(Visit(decl->fun()));
}


void AsmWasmBuilder::VisitImportDeclaration(ImportDeclaration* decl) {}


void AsmWasmBuilder::VisitExportDeclaration(ExportDeclaration* decl) {}


void AsmWasmBuilder::VisitStatements(ZoneList<Statement*>* stmts) {
  for (int i = 0; i < stmts->length(); ++i) {
    Statement* stmt = stmts->at(i);
    RECURSE(Visit(stmt));
    if (stmt->IsJump()) break;
  }
}


void AsmWasmBuilder::VisitBlock(Block* stmt) {
  RECURSE(VisitStatements(stmt->statements()));
}


void AsmWasmBuilder::VisitExpressionStatement(ExpressionStatement* stmt) {
  RECURSE(Visit(stmt->expression()));
}


void AsmWasmBuilder::VisitEmptyStatement(EmptyStatement* stmt) {}


void AsmWasmBuilder::VisitEmptyParentheses(EmptyParentheses* paren) {
  UNREACHABLE();
}


void AsmWasmBuilder::VisitIfStatement(IfStatement* stmt) {
  RECURSE(Visit(stmt->condition()));
  RECURSE(Visit(stmt->then_statement()));
  RECURSE(Visit(stmt->else_statement()));
}


void AsmWasmBuilder::VisitContinueStatement(ContinueStatement* stmt) {}


void AsmWasmBuilder::VisitBreakStatement(BreakStatement* stmt) {}


void AsmWasmBuilder::VisitReturnStatement(ReturnStatement* stmt) {
  RECURSE(Visit(stmt->expression()));
}


void AsmWasmBuilder::VisitWithStatement(WithStatement* stmt) {
  RECURSE(stmt->expression());
  RECURSE(stmt->statement());
}


void AsmWasmBuilder::VisitSwitchStatement(SwitchStatement* stmt) {
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


void AsmWasmBuilder::VisitCaseClause(CaseClause* clause) { UNREACHABLE(); }


void AsmWasmBuilder::VisitDoWhileStatement(DoWhileStatement* stmt) {
  RECURSE(Visit(stmt->body()));
  RECURSE(Visit(stmt->cond()));
}


void AsmWasmBuilder::VisitWhileStatement(WhileStatement* stmt) {
  RECURSE(Visit(stmt->cond()));
  RECURSE(Visit(stmt->body()));
}


void AsmWasmBuilder::VisitForStatement(ForStatement* stmt) {
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


void AsmWasmBuilder::VisitForInStatement(ForInStatement* stmt) {
  RECURSE(Visit(stmt->enumerable()));
  RECURSE(Visit(stmt->body()));
}


void AsmWasmBuilder::VisitForOfStatement(ForOfStatement* stmt) {
  RECURSE(Visit(stmt->iterable()));
  RECURSE(Visit(stmt->body()));
}


void AsmWasmBuilder::VisitTryCatchStatement(TryCatchStatement* stmt) {
  RECURSE(Visit(stmt->try_block()));
  RECURSE(Visit(stmt->catch_block()));
}


void AsmWasmBuilder::VisitTryFinallyStatement(TryFinallyStatement* stmt) {
  RECURSE(Visit(stmt->try_block()));
  RECURSE(Visit(stmt->finally_block()));
}


void AsmWasmBuilder::VisitDebuggerStatement(DebuggerStatement* stmt) {}


void AsmWasmBuilder::VisitFunctionLiteral(FunctionLiteral* expr) {
  Scope* scope = expr->scope();
  RECURSE(VisitDeclarations(scope->declarations()));
  RECURSE(VisitStatements(expr->body()));
}


void AsmWasmBuilder::VisitNativeFunctionLiteral(NativeFunctionLiteral* expr) {}


void AsmWasmBuilder::VisitConditional(Conditional* expr) {
  RECURSE(Visit(expr->condition()));
  RECURSE(Visit(expr->then_expression()));
  RECURSE(Visit(expr->else_expression()));
}


void AsmWasmBuilder::VisitVariableProxy(VariableProxy* expr) {}


void AsmWasmBuilder::VisitLiteral(Literal* expr) {}


void AsmWasmBuilder::VisitRegExpLiteral(RegExpLiteral* expr) {}


void AsmWasmBuilder::VisitObjectLiteral(ObjectLiteral* expr) {
  ZoneList<ObjectLiteralProperty*>* props = expr->properties();
  for (int i = 0; i < props->length(); ++i) {
    ObjectLiteralProperty* prop = props->at(i);
    RECURSE(Visit(prop->value()));
  }
}


void AsmWasmBuilder::VisitArrayLiteral(ArrayLiteral* expr) {
  ZoneList<Expression*>* values = expr->values();
  for (int i = 0; i < values->length(); ++i) {
    Expression* value = values->at(i);
    RECURSE(Visit(value));
  }
}


void AsmWasmBuilder::VisitAssignment(Assignment* expr) {
  RECURSE(Visit(expr->target()));
  RECURSE(Visit(expr->value()));
}


void AsmWasmBuilder::VisitYield(Yield* expr) {
  RECURSE(Visit(expr->generator_object()));
  RECURSE(Visit(expr->expression()));
}


void AsmWasmBuilder::VisitThrow(Throw* expr) {
  RECURSE(Visit(expr->exception()));
}


void AsmWasmBuilder::VisitProperty(Property* expr) {
  RECURSE(Visit(expr->obj()));
  RECURSE(Visit(expr->key()));
}


void AsmWasmBuilder::VisitCall(Call* expr) {
  RECURSE(Visit(expr->expression()));
  ZoneList<Expression*>* args = expr->arguments();
  for (int i = 0; i < args->length(); ++i) {
    Expression* arg = args->at(i);
    RECURSE(Visit(arg));
  }
}


void AsmWasmBuilder::VisitCallNew(CallNew* expr) {
  RECURSE(Visit(expr->expression()));
  ZoneList<Expression*>* args = expr->arguments();
  for (int i = 0; i < args->length(); ++i) {
    Expression* arg = args->at(i);
    RECURSE(Visit(arg));
  }
}


void AsmWasmBuilder::VisitCallRuntime(CallRuntime* expr) {
  ZoneList<Expression*>* args = expr->arguments();
  for (int i = 0; i < args->length(); ++i) {
    Expression* arg = args->at(i);
    RECURSE(Visit(arg));
  }
}


void AsmWasmBuilder::VisitUnaryOperation(UnaryOperation* expr) {
  RECURSE(Visit(expr->expression()));
}


void AsmWasmBuilder::VisitCountOperation(CountOperation* expr) {
  RECURSE(Visit(expr->expression()));
}


void AsmWasmBuilder::VisitBinaryOperation(BinaryOperation* expr) {
  fprintf(stderr, "Doing op '%s'\n", Token::Name(expr->op()));
  RECURSE(Visit(expr->left()));
  RECURSE(Visit(expr->right()));
}


void AsmWasmBuilder::VisitCompareOperation(CompareOperation* expr) {
  RECURSE(Visit(expr->left()));
  RECURSE(Visit(expr->right()));
}


void AsmWasmBuilder::VisitThisFunction(ThisFunction* expr) {}


void AsmWasmBuilder::VisitDeclarations(ZoneList<Declaration*>* decls) {
  for (int i = 0; i < decls->length(); ++i) {
    Declaration* decl = decls->at(i);
    RECURSE(Visit(decl));
  }
}


void AsmWasmBuilder::VisitClassLiteral(ClassLiteral* expr) {}


void AsmWasmBuilder::VisitSpread(Spread* expr) {}


void AsmWasmBuilder::VisitSuperPropertyReference(SuperPropertyReference* expr) {
}


void AsmWasmBuilder::VisitSuperCallReference(SuperCallReference* expr) {}

void AsmWasmBuilder::VisitSloppyBlockFunctionStatement(
    SloppyBlockFunctionStatement* expr) {}


}  // namespace wasm
}  // namespace internal
}  // namespace v8
