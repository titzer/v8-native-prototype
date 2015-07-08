// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-opcodes.h"
#include "src/signature.h"

namespace v8 {
namespace internal {
namespace wasm {

typedef Signature<LocalType> FunctionSig;

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


const char* WasmOpcodes::TypeName(LocalType type) {
  switch (type) {
    case kAstStmt:
      return "<stmt>";
    case kAstInt32:
      return "int32";
    case kAstInt64:
      return "int64";
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
    case kMemInt64:
      return "int64";
    case kMemUint64:
      return "uint64";
    case kMemFloat32:
      return "float32";
    case kMemFloat64:
      return "float64";
    default:
      return "Unknown";
  }
}


#define DECLARE_SIG_ENUM(name, ...) kSigEnum_##name,

enum WasmOpcodeSig { FOREACH_SIGNATURE(DECLARE_SIG_ENUM) };

// TODO(titzer): not static-initializer safe. Wrap in LazyInstance.
#define DECLARE_SIG(name, ...)                      \
  static LocalType kTypes_##name[] = {__VA_ARGS__}; \
  static const FunctionSig kSig_##name(             \
      1, static_cast<int>(arraysize(kTypes_##name)) - 1, kTypes_##name);

FOREACH_SIGNATURE(DECLARE_SIG)

#define DECLARE_SIG_ENTRY(name, ...) &kSig_##name,

static const FunctionSig* kSimpleExprSigs[] = {
    nullptr, FOREACH_SIGNATURE(DECLARE_SIG_ENTRY)};

static byte kSimpleExprSigTable[256];


// Initialize the signature table.
static void InitSigTable() {
#define SET_SIG_TABLE(name, opcode, sig) \
  kSimpleExprSigTable[opcode] = static_cast<int>(kSigEnum_##sig) + 1;
  FOREACH_SIMPLE_EXPR_OPCODE(SET_SIG_TABLE);
#undef SET_SIG_TABLE
}


FunctionSig* WasmOpcodes::Signature(WasmOpcode opcode) {
  // TODO(titzer): use LazyInstance to make this thread safe.
  if (kSimpleExprSigTable[kExprInt32Add] == 0) InitSigTable();
  return const_cast<FunctionSig*>(
      kSimpleExprSigs[kSimpleExprSigTable[static_cast<byte>(opcode)]]);
}


bool WasmOpcodes::IsSupported(WasmOpcode opcode) {
  switch (opcode) {
#if !V8_TURBOFAN_BACKEND_64
    // Opcodes not supported on 32-bit platforms.
    case kExprInt64LoadMemL:
    case kExprInt32LoadMemH:
    case kExprInt64LoadMemH:
    case kExprFloat32LoadMemH:
    case kExprFloat64LoadMemH:
    case kExprInt64StoreMemL:
    case kExprInt32StoreMemH:
    case kExprInt64StoreMemH:
    case kExprFloat32StoreMemH:
    case kExprFloat64StoreMemH:

    case kExprInt64Add:
    case kExprInt64Sub:
    case kExprInt64Mul:
    case kExprInt64SDiv:
    case kExprInt64UDiv:
    case kExprInt64SRem:
    case kExprInt64URem:
    case kExprInt64And:
    case kExprInt64Ior:
    case kExprInt64Xor:
    case kExprInt64Shl:
    case kExprInt64Shr:
    case kExprInt64Sar:
    case kExprInt64Eq:
    case kExprInt64Slt:
    case kExprInt64Sle:
    case kExprInt64Ult:
    case kExprInt64Ule:
    case kExprInt64Sgt:
    case kExprInt64Sge:
    case kExprInt64Ugt:
    case kExprInt64Uge:

    case kExprInt32ConvertInt64:
    case kExprInt64SConvertInt32:
    case kExprInt64UConvertInt32:
#endif

    case kExprInt32Clz:
    case kExprInt32Ctz:
    case kExprInt32PopCnt:

    case kExprInt64Clz:
    case kExprInt64Ctz:
    case kExprInt64PopCnt:

    case kExprFloat32Min:
    case kExprFloat32Max:
    case kExprFloat32Neg:
    case kExprFloat32CopySign:
    case kExprFloat32Ceil:
    case kExprFloat32Floor:
    case kExprFloat32Trunc:
    case kExprFloat32NearestInt:
    case kExprFloat32Sqrt:

    case kExprFloat64Min:
    case kExprFloat64Max:
    case kExprFloat64Neg:
    case kExprFloat64CopySign:
    case kExprFloat64Ceil:
    case kExprFloat64Floor:
    case kExprFloat64Trunc:
    case kExprFloat64NearestInt:
    case kExprFloat64Sqrt:

    case kExprInt64SConvertFloat32:
    case kExprInt64SConvertFloat64:
    case kExprInt64UConvertFloat32:
    case kExprInt64UConvertFloat64:
    case kExprFloat32SConvertInt64:
    case kExprFloat32UConvertInt64:
    case kExprFloat32ReinterpretInt32:
    case kExprFloat64SConvertInt64:
    case kExprFloat64UConvertInt64:
    case kExprFloat64ReinterpretInt64:
      return false;
    default:
      return true;
  }
}
}
}
}
