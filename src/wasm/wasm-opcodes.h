// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_OPCODES_H_
#define V8_WASM_OPCODES_H_

#include "src/signature.h"

namespace v8 {
namespace internal {
namespace wasm {

// Types for syntax tree nodes.
enum LocalType {
  kAstStmt = 0,     // a statement node
  kAstInt32 = 1,    // expression that produces an int32 value
  kAstInt64 = 2,    // expression that produces an int64 value
  kAstFloat32 = 3,  // expression that produces a float32 value
  kAstFloat64 = 4   // expression that produces a float64 value
};

// Types for memory accesses and globals.
enum MemType {
  kMemInt8 = 0,
  kMemUint8 = 1,
  kMemInt16 = 2,
  kMemUint16 = 3,
  kMemInt32 = 4,
  kMemUint32 = 5,
  kMemInt64 = 6,
  kMemUint64 = 7,
  kMemFloat32 = 8,
  kMemFloat64 = 9
};

// Atomicity annotations for access to the memory and globals.
enum Atomicity {
  kNone = 0,        // non-atomic
  kSequential = 1,  // sequential consistency
  kAcquire = 2,     // acquire semantics
  kRelease = 3      // release semantics
};

typedef Signature<LocalType> FunctionSig;

// Statements.
#define FOREACH_STMT_OPCODE(V) \
  V(Nop, 0x00, _)              \
  V(SetLocal, 0x01, _)         \
  V(StoreGlobal, 0x02, _)      \
  V(StoreMem, 0x03, _)         \
  V(If, 0x04, _)               \
  V(IfThen, 0x05, _)           \
  V(Block, 0x06, _)            \
  V(Switch, 0x07, _)           \
  V(SwitchNf, 0x08, _)         \
  V(Loop, 0x09, _)             \
  V(Continue, 0x0a, _)         \
  V(Break, 0x0b, _)            \
  V(Return, 0x0c, _)

// Miscellaenous and polymorphic expressions.
#define FOREACH_MISC_EXPR_OPCODE(V) \
  V(Int8Const, 0x10, _)             \
  V(Int32Const, 0x11, _)            \
  V(Float64Const, 0x12, _)          \
  V(Float32Const, 0x13, _)          \
  V(GetLocal, 0x14, _)              \
  V(LoadGlobal, 0x15, _)            \
  V(LoadMem, 0x16, _)               \
  V(CallFunction, 0x17, _)          \
  V(CallIndirect, 0x18, _)          \
  V(Ternary, 0x19, _)               \
  V(Comma, 0x1a, _)

// Simple expressions.
#define FOREACH_SIMPLE_EXPR_OPCODE(V) \
  V(Int32Add, 0x20, i_ii)             \
  V(Int32Sub, 0x21, i_ii)             \
  V(Int32Mul, 0x22, i_ii)             \
  V(Int32SDiv, 0x23, i_ii)            \
  V(Int32UDiv, 0x24, i_ii)            \
  V(Int32SRem, 0x25, i_ii)            \
  V(Int32URem, 0x26, i_ii)            \
  V(Int32And, 0x27, i_ii)             \
  V(Int32Ior, 0x28, i_ii)             \
  V(Int32Xor, 0x29, i_ii)             \
  V(Int32Shl, 0x2a, i_ii)             \
  V(Int32Shr, 0x2b, i_ii)             \
  V(Int32Sar, 0x2c, i_ii)             \
  V(Int32Eq, 0x2d, i_ii)              \
  V(Int32Slt, 0x2e, i_ii)             \
  V(Int32Sle, 0x2f, i_ii)             \
  V(Int32Ult, 0x30, i_ii)             \
  V(Int32Ule, 0x31, i_ii)             \
  V(Int32Clz, 0x32, i_i)              \
  V(Int32Ctz, 0x33, i_i)              \
  V(Int32PopCnt, 0x34, i_i)           \
  V(BoolNot, 0x35, i_i)               \
  V(Float64Add, 0x40, d_dd)           \
  V(Float64Sub, 0x41, d_dd)           \
  V(Float64Mul, 0x42, d_dd)           \
  V(Float64Div, 0x43, d_dd)           \
  V(Float64Min, 0x44, d_dd)           \
  V(Float64Max, 0x45, d_dd)           \
  V(Float64Abs, 0x46, d_d)            \
  V(Float64Neg, 0x47, d_d)            \
  V(Float64CopySign, 0x48, d_d)       \
  V(Float64Ceil, 0x49, d_d)           \
  V(Float64Floor, 0x4a, d_d)          \
  V(Float64Trunc, 0x4b, d_d)          \
  V(Float64NearestInt, 0x4c, d_d)     \
  V(Float64Sqrt, 0x4d, d_d)           \
  V(Float64Eq, 0x4e, i_dd)            \
  V(Float64Lt, 0x4f, i_dd)            \
  V(Float64Le, 0x50, i_dd)            \
  V(Float32Add, 0x60, f_ff)           \
  V(Float32Sub, 0x61, f_ff)           \
  V(Float32Mul, 0x62, f_ff)           \
  V(Float32Div, 0x63, f_ff)           \
  V(Float32Min, 0x64, f_ff)           \
  V(Float32Max, 0x65, f_ff)           \
  V(Float32Abs, 0x66, f_f)            \
  V(Float32Neg, 0x67, f_f)            \
  V(Float32CopySign, 0x68, f_f)       \
  V(Float32Ceil, 0x69, f_f)           \
  V(Float32Floor, 0x6a, f_f)          \
  V(Float32Trunc, 0x6b, f_f)          \
  V(Float32NearestInt, 0x6c, f_f)     \
  V(Float32Sqrt, 0x6d, f_f)           \
  V(Float32Eq, 0x6e, i_ff)            \
  V(Float32Lt, 0x6f, i_ff)            \
  V(Float32Le, 0x70, i_ff)            \
  V(Int32FromFloat32, 0x80, i_f)      \
  V(Int32FromFloat64, 0x81, i_d)      \
  V(Uint32FromFloat32, 0x82, i_f)     \
  V(Uint32FromFloat64, 0x83, i_d)     \
  V(Float64FromSInt32, 0x84, d_i)     \
  V(Float64FromUInt32, 0x85, d_i)     \
  V(Float64FromFloat32, 0x86, d_f)    \
  V(Float32FromSInt32, 0x87, f_i)     \
  V(Float32FromUInt32, 0x88, f_i)     \
  V(Float32FromFloat64, 0x89, f_d)

// All expression opcodes.
#define FOREACH_EXPR_OPCODE(V) \
  FOREACH_MISC_EXPR_OPCODE(V)  \
  FOREACH_SIMPLE_EXPR_OPCODE(V)

// All opcodes.
#define FOREACH_OPCODE(V) \
  FOREACH_STMT_OPCODE(V)  \
  FOREACH_EXPR_OPCODE(V)

enum WasmOpcode {
// Declare statement opcodes.
#define DECLARE_NAMED_ENUM(name, opcode, sig) kStmt##name = opcode,
  FOREACH_STMT_OPCODE(DECLARE_NAMED_ENUM)
#undef DECLARE_NAMED_ENUM

// Declare expression opcodes.
#define DECLARE_NAMED_ENUM(name, opcode, sig) kExpr##name = opcode,
      FOREACH_EXPR_OPCODE(DECLARE_NAMED_ENUM)
#undef DECLARE_NAMED_ENUM
};

// A collection of opcode-related static methods.
class WasmOpcodes {
 public:
  static bool IsSupported(WasmOpcode opcode);
  static const char* OpcodeName(WasmOpcode opcode);
  static const char* TypeName(LocalType type);
  static const char* TypeName(MemType type);
  static FunctionSig* Signature(WasmOpcode opcode);

  static LocalType LocalTypeFor(MemType type) {
    switch (type) {
      case kMemInt8:
      case kMemUint8:
      case kMemInt16:
      case kMemUint16:
      case kMemInt32:
      case kMemUint32:
        return kAstInt32;
      case kMemInt64:
      case kMemUint64:
        return kAstInt64;
      case kMemFloat32:
        return kAstFloat32;
      case kMemFloat64:
        return kAstFloat64;
    }
  }
};
}
}
}

#endif  // V8_WASM_OPCODES_H_
