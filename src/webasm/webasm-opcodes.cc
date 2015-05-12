// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/webasm/webasm-opcodes.h"

namespace v8 {
namespace internal {
namespace webasm {

const char* OpcodeName(WebAsmOpcode opcode) {
  switch (opcode) {
#define DECLARE_NAME_CASE(name, opcode) \
  case kStmt##name:                     \
    return "Stmt" #name;
    FOREACH_STMT_OPCODE(DECLARE_NAME_CASE)
#undef DECLARE_NAME_CASE

#define DECLARE_NAME_CASE(name, opcode) \
  case kExpr##name:                     \
    return "Expr" #name;
    FOREACH_EXPR_OPCODE(DECLARE_NAME_CASE)
#undef DECLARE_NAME_CASE
    default:
      break;
  }
  return "Unknown";
}


const char* TypeName(AstType type) {
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


const char* TypeName(MemType type) {
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
}
}
}
