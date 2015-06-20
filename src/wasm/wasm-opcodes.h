// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WEBASM_OPCODES_H_
#define V8_WEBASM_OPCODES_H_

namespace v8 {
namespace internal {
namespace wasm {

// Types for syntax tree nodes.
enum AstType {
  kAstStmt = 0,     // a statement node
  kAstInt32 = 1,    // expression that produces an int32 value
  kAstFloat32 = 2,  // expression that produces a float32 value
  kAstFloat64 = 3   // expression that produces a float64 value
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
  V(Nop, 0x00)                 \
  V(SetLocal, 0x01)            \
  V(SetGlobal, 0x02)           \
  V(SetHeap, 0x03)             \
  V(If, 0x04)                  \
  V(IfThen, 0x05)              \
  V(Block, 0x06)               \
  V(Switch, 0x07)              \
  V(SwitchNf, 0x08)            \
  V(Loop, 0x09)                \
  V(Continue, 0x0a)            \
  V(Break, 0x0b)               \
  V(Return, 0x0c)

#define FOREACH_EXPR_OPCODE_MISC(V) \
  V(Int8Const, 0x10)                \
  V(Int32Const, 0x11)               \
  V(Float64Const, 0x12)             \
  V(Float32Const, 0x13)             \
  V(GetLocal, 0x14)                 \
  V(GetGlobal, 0x15)                \
  V(GetHeap, 0x16)                  \
  V(CallFunction, 0x17)             \
  V(CallIndirect, 0x18)             \
  V(Ternary, 0x19)                  \
  V(Comma, 0x1a)                    \
  V(BoolNot, 0x1b)

// Integer binops.
#define FOREACH_I_II_OPCODE(V) \
  V(Int32Add, 0x20)            \
  V(Int32Sub, 0x21)            \
  V(Int32Mul, 0x22)            \
  V(Int32SDiv, 0x23)           \
  V(Int32UDiv, 0x24)           \
  V(Int32SRem, 0x25)           \
  V(Int32URem, 0x26)           \
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

// Integer unops.
#define FOREACH_I_I_OPCODE(V) \
  V(Int32Clz, 0x32)           \
  V(Int32Ctz, 0x33)           \
  V(Int32PopCnt, 0x34)

// Float64 binops that produce float64.
#define FOREACH_D_DD_OPCODE(V) \
  V(Float64Add, 0x40)          \
  V(Float64Sub, 0x41)          \
  V(Float64Mul, 0x42)          \
  V(Float64Div, 0x43)          \
  V(Float64Min, 0x44)          \
  V(Float64Max, 0x45)

// Float64 unops.
#define FOREACH_D_D_OPCODE(V) \
  V(Float64Abs, 0x46)         \
  V(Float64Neg, 0x47)         \
  V(Float64CopySign, 0x48)    \
  V(Float64Ceil, 0x49)        \
  V(Float64Floor, 0x4a)       \
  V(Float64Trunc, 0x4b)       \
  V(Float64NearestInt, 0x4c)  \
  V(Float64Sqrt, 0x4d)

// Float64 binops that produce int32.
#define FOREACH_I_DD_OPCODE(V) \
  V(Float64Eq, 0x4e)           \
  V(Float64Lt, 0x4f)           \
  V(Float64Le, 0x50)

// Float32 binops that produce float32.
#define FOREACH_F_FF_OPCODE(V) \
  V(Float32Add, 0x60)          \
  V(Float32Sub, 0x61)          \
  V(Float32Mul, 0x62)          \
  V(Float32Div, 0x63)          \
  V(Float32Min, 0x64)          \
  V(Float32Max, 0x65)

// Float32 unops.
#define FOREACH_F_F_OPCODE(V) \
  V(Float32Abs, 0x66)         \
  V(Float32Neg, 0x67)         \
  V(Float32CopySign, 0x68)    \
  V(Float32Ceil, 0x69)        \
  V(Float32Floor, 0x6a)       \
  V(Float32Trunc, 0x6b)       \
  V(Float32NearestInt, 0x6c)  \
  V(Float32Sqrt, 0x6d)

// Float32 binops that produce int32.
#define FOREACH_I_FF_OPCODE(V) \
  V(Float32Eq, 0x6e)           \
  V(Float32Lt, 0x6f)           \
  V(Float32Le, 0x70)

// Conversions between primitive types.
#define FOREACH_CONVERSION_OPCODE(V) \
  V(Int32FromFloat32, 0x80)          \
  V(Int32FromFloat64, 0x81)          \
  V(Uint32FromFloat32, 0x82)         \
  V(Uint32FromFloat64, 0x83)         \
  V(Float64FromSInt32, 0x84)         \
  V(Float64FromUInt32, 0x85)         \
  V(Float64FromFloat32, 0x86)        \
  V(Float32FromSInt32, 0x87)         \
  V(Float32FromUInt32, 0x88)         \
  V(Float32FromFloat64, 0x89)

// Expression opcodes.
#define FOREACH_EXPR_OPCODE(V) \
  FOREACH_EXPR_OPCODE_MISC(V)  \
  FOREACH_I_II_OPCODE(V)       \
  FOREACH_D_DD_OPCODE(V)       \
  FOREACH_D_D_OPCODE(V)        \
  FOREACH_I_DD_OPCODE(V)       \
  FOREACH_F_FF_OPCODE(V)       \
  FOREACH_F_F_OPCODE(V)        \
  FOREACH_I_FF_OPCODE(V)       \
  FOREACH_CONVERSION_OPCODE(V)

// All opcodes.
#define FOREACH_OPCODE(V) \
  FOREACH_STMT_OPCODE(V)  \
  FOREACH_EXPR_OPCODE(V)

enum WasmOpcode {
// Declare statement opcodes.
#define DECLARE_NAMED_ENUM(name, opcode) kStmt##name = opcode,
  FOREACH_STMT_OPCODE(DECLARE_NAMED_ENUM)
#undef DECLARE_NAMED_ENUM

// Declare expression opcodes.
#define DECLARE_NAMED_ENUM(name, opcode) kExpr##name = opcode,
      FOREACH_EXPR_OPCODE(DECLARE_NAMED_ENUM)
#undef DECLARE_NAMED_ENUM
};

const char* OpcodeName(WasmOpcode opcode);
const char* TypeName(AstType type);
const char* TypeName(MemType type);
}
}
}

#endif  // V8_WEBASM_OPCODES_H_
