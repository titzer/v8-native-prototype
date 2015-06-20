// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-opcodes.h"
#include "src/signature.h"

namespace v8 {
namespace internal {
namespace wasm {

typedef Signature<AstType> FunctionSig;

const char* WasmOpcodes::OpcodeName(WasmOpcode opcode) {
  switch (opcode) {
#define DECLARE_NAME_CASE(name, opcode, sig) \
  case kStmt##name:                          \
    return "Stmt" #name;
    FOREACH_STMT_OPCODE(DECLARE_NAME_CASE)
#undef DECLARE_NAME_CASE

#define DECLARE_NAME_CASE(name, opcode, sig) \
  case kExpr##name:                          \
    return "Expr" #name;
    FOREACH_EXPR_OPCODE(DECLARE_NAME_CASE)
#undef DECLARE_NAME_CASE
    default:
      break;
  }
  return "Unknown";
}


const char* WasmOpcodes::TypeName(AstType type) {
  switch (type) {
    case kAstStmt:
      return "<stmt>";
    case kAstInt32:
      return "int32";
    case kAstFloat32:
      return "float32";
    case kAstFloat64:
      return "float64";
    default:
      return "Unknown";
  }
}


const char* WasmOpcodes::TypeName(MemType type) {
  switch (type) {
    case kMemInt8:
      return "int8";
    case kMemUint8:
      return "uint8";
    case kMemInt16:
      return "int16";
    case kMemUint16:
      return "uint16";
    case kMemInt32:
      return "int32";
    case kMemUint32:
      return "uint32";
    case kMemFloat32:
      return "float32";
    case kMemFloat64:
      return "float64";
    default:
      return "Unknown";
  }
}


// TODO(titzer): not static-initializer safe. Wrap in LazyInstance.
#define DECLARE_SIG(name, index, ...)             \
  static const int kSigIndex_##name = index;      \
  static AstType kTypes_##name[] = {__VA_ARGS__}; \
  static const FunctionSig kSig_##name(           \
      1, static_cast<int>(arraysize(kTypes_##name)) - 1, kTypes_##name)

DECLARE_SIG(i_ii, 1, kAstInt32, kAstInt32, kAstInt32);
DECLARE_SIG(i_i, 2, kAstInt32, kAstInt32);
DECLARE_SIG(i_ff, 3, kAstInt32, kAstFloat32, kAstFloat32);
DECLARE_SIG(i_f, 4, kAstInt32, kAstFloat32);
DECLARE_SIG(i_dd, 5, kAstInt32, kAstFloat64, kAstFloat64);
DECLARE_SIG(i_d, 6, kAstInt32, kAstFloat64);
DECLARE_SIG(f_ff, 7, kAstFloat32, kAstFloat32, kAstFloat32);
DECLARE_SIG(f_f, 8, kAstFloat32, kAstFloat32);
DECLARE_SIG(f_d, 9, kAstFloat32, kAstFloat64);
DECLARE_SIG(f_i, 10, kAstFloat32, kAstInt32);
DECLARE_SIG(d_dd, 11, kAstFloat64, kAstFloat64, kAstFloat64);
DECLARE_SIG(d_d, 12, kAstFloat64, kAstFloat64);
DECLARE_SIG(d_f, 13, kAstFloat64, kAstFloat32);
DECLARE_SIG(d_i, 14, kAstFloat64, kAstInt32);

static const FunctionSig* kSimpleExprSigs[16] = {
    nullptr,    &kSig_i_ii, &kSig_i_i,  &kSig_i_ff, &kSig_i_f,
    &kSig_i_dd, &kSig_i_d,  &kSig_f_ff, &kSig_f_f,  &kSig_f_d,
    &kSig_f_i,  &kSig_d_dd, &kSig_d_d,  &kSig_d_f,  &kSig_d_i};

static byte kSimpleExprSigTable[256];

// Initialize the signature table.
static void InitSigTable() {
#define SET_SIG_TABLE(name, opcode, sig) \
  kSimpleExprSigTable[opcode] = kSigIndex_##sig;
  FOREACH_SIMPLE_EXPR_OPCODE(SET_SIG_TABLE);
#undef SET_SIG_TABLE
}

const void* WasmOpcodes::Signature(WasmOpcode opcode) {
  // TODO(titzer): use LazyInstance to make this thread safe.
  if (kSimpleExprSigTable[kExprInt32Add] == 0) InitSigTable();
  return kSimpleExprSigs[kSimpleExprSigTable[static_cast<byte>(opcode)]];
}
}
}
}
