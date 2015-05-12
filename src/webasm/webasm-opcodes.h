// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WEBASM_OPCODES_H_
#define V8_WEBASM_OPCODES_H_

namespace v8 {
namespace internal {
namespace webasm {

// Types for syntax tree nodes.
enum AstType {
  kAstStmt = 0,     // a statement node
  kAstInt32 = 1,    // expression that produces an int32 value
  kAstFloat64 = 2,  // expression that produces a float64 value
  kAstFloat32 = 3   // expression that produces a float32 value
};

// Types for heap accesses and globals.
enum MemType {
  kMemInt8 = 0,
  kMemUint8 = 1,
  kMemInt16 = 2,
  kMemUint16 = 3,
  kMemInt32 = 4,
  kMemUint32 = 5,
  kMemFloat32 = 6,
  kMemFloat64 = 7
};

// Atomicity annotations for access to the heap and globals.
enum Atomicity {
  kNone = 0,        // non-atomic
  kSequential = 1,  // sequential consistency
  kAcquire = 2,     // acquire semantics
  kRelease = 3      // release semantics
};

#define FOREACH_STMT_OPCODE(V) \
  V(SetLocal, 0x00)            \
  V(SetGlobal, 0x01)           \
  V(SetHeap, 0x02)             \
  V(If, 0x03)                  \
  V(IfThen, 0x04)              \
  V(Block, 0x05)               \
  V(Loop, 0x06)                \
  V(Continue, 0x07)            \
  V(Break, 0x08)               \
  V(Return, 0x09)

#define FOREACH_EXPR_OPCODE_MISC(V) \
  V(Int8Const, 0x10)                \
  V(Int32Const, 0x11)               \
  V(Float64Const, 0x12)             \
  V(Float32Const, 0x13)             \
  V(GetLocal, 0x14)                 \
  V(GetGlobal, 0x15)                \
  V(GetHeap, 0x16)                  \
  V(CallFunction, 0x17)             \
  V(CallIndirect, 0x18)

// Integer binops.
#define FOREACH_I_II_OPCODE(V) \
  V(Int32Add, 0x20)            \
  V(Int32Sub, 0x21)            \
  V(Int32Mul, 0x22)            \
  V(Int32SDiv, 0x23)           \
  V(Int32UDiv, 0x24)           \
  V(Int32SMod, 0x25)           \
  V(Int32UMod, 0x26)           \
  V(Int32And, 0x27)            \
  V(Int32Ior, 0x28)            \
  V(Int32Xor, 0x29)            \
  V(Int32Shl, 0x2a)            \
  V(Int32Shr, 0x2b)            \
  V(Int32Sar, 0x2c)            \
  V(Int32Eq, 0x2d)             \
  V(Int32Slt, 0x2e)            \
  V(Int32Sle, 0x2f)            \
  V(Int32Ult, 0x30)            \
  V(Int32Ule, 0x31)

// Float64 binops that produce float64.
#define FOREACH_D_DD_OPCODE(V) \
  V(Float64Add, 0x40)          \
  V(Float64Sub, 0x41)          \
  V(Float64Mul, 0x42)          \
  V(Float64Div, 0x43)          \
  V(Float64Mod, 0x44)

// Float64 binops that produce int32.
#define FOREACH_I_DD_OPCODE(V) \
  V(Float64Eq, 0x45)           \
  V(Float64Lt, 0x46)           \
  V(Float64Le, 0x47)

// Float32 binops that produce float32.
#define FOREACH_F_FF_OPCODE(V) \
  V(Float32Add, 0x50)          \
  V(Float32Sub, 0x51)          \
  V(Float32Mul, 0x52)          \
  V(Float32Div, 0x53)          \
  V(Float32Mod, 0x54)

// Float32 binops that produce int32.
#define FOREACH_I_FF_OPCODE(V) \
  V(Float32Eq, 0x55)           \
  V(Float32Lt, 0x56)           \
  V(Float32Le, 0x57)

// Conversions between primitive types.
#define FOREACH_CONVERSION_OPCODE(V) \
  V(Int32FromFloat32, 0x60)          \
  V(Int32FromFloat64, 0x61)          \
  V(Uint32FromFloat32, 0x62)         \
  V(Uint32FromFloat64, 0x63)         \
  V(Float64FromSInt32, 0x64)         \
  V(Float64FromUInt32, 0x65)         \
  V(Float64FromFloat32, 0x66)        \
  V(Float32FromSInt32, 0x67)         \
  V(Float32FromUInt32, 0x68)         \
  V(Float32FromFloat64, 0x69)

// Expression opcodes.
#define FOREACH_EXPR_OPCODE(V) \
  FOREACH_EXPR_OPCODE_MISC(V)  \
  FOREACH_I_II_OPCODE(V)       \
  FOREACH_D_DD_OPCODE(V)       \
  FOREACH_I_DD_OPCODE(V)       \
  FOREACH_F_FF_OPCODE(V)       \
  FOREACH_I_FF_OPCODE(V)

// All opcodes.
#define FOREACH_OPCODE(V) \
  FOREACH_STMT_OPCODE(V)  \
  FOREACH_EXPR_OPCODE(V)

enum WebAsmOpcode {
// Declare statement opcodes.
#define DECLARE_NAMED_ENUM(name, opcode) kStmt##name = opcode,
  FOREACH_STMT_OPCODE(DECLARE_NAMED_ENUM)
#undef DECLARE_NAMED_ENUM

// Declare expression opcodes.
#define DECLARE_NAMED_ENUM(name, opcode) kExpr##name = opcode,
      FOREACH_EXPR_OPCODE(DECLARE_NAMED_ENUM)
#undef DECLARE_NAMED_ENUM
};

const char* OpcodeName(WebAsmOpcode opcode);
const char* TypeName(AstType type);
const char* TypeName(MemType type);
}
}
}

#endif  // V8_WEBASM_OPCODES_H_
