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

// Functionality related to encoding memory accesses.
struct MemoryAccess {
  // Atomicity annotations for access to the memory and globals.
  enum Atomicity {
    kNone = 0,        // non-atomic
    kSequential = 1,  // sequential consistency
    kAcquire = 2,     // acquire semantics
    kRelease = 3      // release semantics
  };

  // Alignment annotations for memory accesses.
  enum Alignment { kAligned = 0, kUnaligned = 1 };

  // Memory width for integer accesses.
  enum IntWidth { kInt8 = 0, kInt16 = 1, kInt32 = 2, kInt64 = 3 };

  // Bitfields for the various annotations for memory accesses.
  typedef BitField<IntWidth, 0, 2> IntWidthField;
  typedef BitField<bool, 2, 1> SignExtendField;
  typedef BitField<Alignment, 3, 1> AlignmentField;
  typedef BitField<Atomicity, 4, 2> AtomicityField;
};

typedef Signature<LocalType> FunctionSig;

// Statements.
#define FOREACH_STMT_OPCODE(V) \
  V(Nop, 0x00, _)              \
  V(If, 0x01, _)               \
  V(IfThen, 0x02, _)           \
  V(Block, 0x03, _)            \
  V(Switch, 0x04, _)           \
  V(SwitchNf, 0x05, _)         \
  V(Loop, 0x06, _)             \
  V(Continue, 0x07, _)         \
  V(Break, 0x08, _)            \
  V(Return, 0x09, _)

// Miscellaenous and polymorphic expressions.
#define FOREACH_MISC_EXPR_OPCODE(V) \
  V(Int8Const, 0x10, _)             \
  V(Int32Const, 0x11, _)            \
  V(Int64Const, 0x12, _)            \
  V(Float64Const, 0x13, _)          \
  V(Float32Const, 0x14, _)          \
  V(GetLocal, 0x15, _)              \
  V(SetLocal, 0x16, _)              \
  V(LoadGlobal, 0x17, _)            \
  V(StoreGlobal, 0x18, _)           \
  V(CallFunction, 0x19, _)          \
  V(CallIndirect, 0x1a, _)          \
  V(Ternary, 0x1b, _)               \
  V(Comma, 0x1c, _)

// Load memory expressions.
#define FOREACH_LOAD_MEM_EXPR_OPCODE(V) \
  V(Int32LoadMemL, 0x20, i_i)           \
  V(Int64LoadMemL, 0x21, l_i)           \
  V(Float32LoadMemL, 0x22, f_i)         \
  V(Float64LoadMemL, 0x23, d_i)         \
  V(Int32LoadMemH, 0x24, i_l)           \
  V(Int64LoadMemH, 0x25, l_l)           \
  V(Float32LoadMemH, 0x26, f_l)         \
  V(Float64LoadMemH, 0x27, d_l)

// Store memory expressions.
#define FOREACH_STORE_MEM_EXPR_OPCODE(V) \
  V(Int32StoreMemL, 0x30, i_ii)          \
  V(Int64StoreMemL, 0x31, l_il)          \
  V(Float32StoreMemL, 0x32, f_if)        \
  V(Float64StoreMemL, 0x33, d_id)        \
  V(Int32StoreMemH, 0x34, i_li)          \
  V(Int64StoreMemH, 0x35, l_ll)          \
  V(Float32StoreMemH, 0x36, f_lf)        \
  V(Float64StoreMemH, 0x37, d_ld)

// Expressions with signatures.
#define FOREACH_SIMPLE_EXPR_OPCODE(V)   \
  V(Int32Add, 0x40, i_ii)               \
  V(Int32Sub, 0x41, i_ii)               \
  V(Int32Mul, 0x42, i_ii)               \
  V(Int32SDiv, 0x43, i_ii)              \
  V(Int32UDiv, 0x44, i_ii)              \
  V(Int32SRem, 0x45, i_ii)              \
  V(Int32URem, 0x46, i_ii)              \
  V(Int32And, 0x47, i_ii)               \
  V(Int32Ior, 0x48, i_ii)               \
  V(Int32Xor, 0x49, i_ii)               \
  V(Int32Shl, 0x4a, i_ii)               \
  V(Int32Shr, 0x4b, i_ii)               \
  V(Int32Sar, 0x4c, i_ii)               \
  V(Int32Eq, 0x4d, i_ii)                \
  V(Int32Ne, 0x4e, i_ii)                \
  V(Int32Slt, 0x4f, i_ii)               \
  V(Int32Sle, 0x50, i_ii)               \
  V(Int32Ult, 0x51, i_ii)               \
  V(Int32Ule, 0x52, i_ii)               \
  V(Int32Sgt, 0x53, i_ii)               \
  V(Int32Sge, 0x54, i_ii)               \
  V(Int32Ugt, 0x55, i_ii)               \
  V(Int32Uge, 0x56, i_ii)               \
  V(Int32Clz, 0x57, i_i)                \
  V(Int32Ctz, 0x58, i_i)                \
  V(Int32PopCnt, 0x59, i_i)             \
  V(BoolNot, 0x5a, i_i)                 \
  V(Int64Add, 0x5b, l_ll)               \
  V(Int64Sub, 0x5c, l_ll)               \
  V(Int64Mul, 0x5d, l_ll)               \
  V(Int64SDiv, 0x5e, l_ll)              \
  V(Int64UDiv, 0x5f, l_ll)              \
  V(Int64SRem, 0x60, l_ll)              \
  V(Int64URem, 0x61, l_ll)              \
  V(Int64And, 0x62, l_ll)               \
  V(Int64Ior, 0x63, l_ll)               \
  V(Int64Xor, 0x64, l_ll)               \
  V(Int64Shl, 0x65, l_ll)               \
  V(Int64Shr, 0x66, l_ll)               \
  V(Int64Sar, 0x67, l_ll)               \
  V(Int64Eq, 0x68, i_ll)                \
  V(Int64Ne, 0x69, i_ll)                \
  V(Int64Slt, 0x6a, i_ll)               \
  V(Int64Sle, 0x6b, i_ll)               \
  V(Int64Ult, 0x6c, i_ll)               \
  V(Int64Ule, 0x6d, i_ll)               \
  V(Int64Sgt, 0x6e, i_ll)               \
  V(Int64Sge, 0x6f, i_ll)               \
  V(Int64Ugt, 0x70, i_ll)               \
  V(Int64Uge, 0x71, i_ll)               \
  V(Int64Clz, 0x72, i_l)                \
  V(Int64Ctz, 0x73, i_l)                \
  V(Int64PopCnt, 0x74, i_l)             \
  V(Float32Add, 0x75, f_ff)             \
  V(Float32Sub, 0x76, f_ff)             \
  V(Float32Mul, 0x77, f_ff)             \
  V(Float32Div, 0x78, f_ff)             \
  V(Float32Min, 0x79, f_ff)             \
  V(Float32Max, 0x7a, f_ff)             \
  V(Float32Abs, 0x7b, f_f)              \
  V(Float32Neg, 0x7c, f_f)              \
  V(Float32CopySign, 0x7d, f_f)         \
  V(Float32Ceil, 0x7e, f_f)             \
  V(Float32Floor, 0x7f, f_f)            \
  V(Float32Trunc, 0x80, f_f)            \
  V(Float32NearestInt, 0x81, f_f)       \
  V(Float32Sqrt, 0x82, f_f)             \
  V(Float32Eq, 0x83, i_ff)              \
  V(Float32Ne, 0x84, i_ff)              \
  V(Float32Lt, 0x85, i_ff)              \
  V(Float32Le, 0x86, i_ff)              \
  V(Float32Gt, 0x87, i_ff)              \
  V(Float32Ge, 0x88, i_ff)              \
  V(Float64Add, 0x89, d_dd)             \
  V(Float64Sub, 0x8a, d_dd)             \
  V(Float64Mul, 0x8b, d_dd)             \
  V(Float64Div, 0x8c, d_dd)             \
  V(Float64Min, 0x8d, d_dd)             \
  V(Float64Max, 0x8e, d_dd)             \
  V(Float64Abs, 0x8f, d_d)              \
  V(Float64Neg, 0x90, d_d)              \
  V(Float64CopySign, 0x91, d_d)         \
  V(Float64Ceil, 0x92, d_d)             \
  V(Float64Floor, 0x93, d_d)            \
  V(Float64Trunc, 0x94, d_d)            \
  V(Float64NearestInt, 0x95, d_d)       \
  V(Float64Sqrt, 0x96, d_d)             \
  V(Float64Eq, 0x97, i_dd)              \
  V(Float64Ne, 0x98, i_dd)              \
  V(Float64Lt, 0x99, i_dd)              \
  V(Float64Le, 0x9a, i_dd)              \
  V(Float64Gt, 0x9b, i_dd)              \
  V(Float64Ge, 0x9c, i_dd)              \
  V(Int32SConvertFloat32, 0x9d, i_f)    \
  V(Int32SConvertFloat64, 0x9e, i_d)    \
  V(Int32UConvertFloat32, 0x9f, i_f)    \
  V(Int32UConvertFloat64, 0xa0, i_d)    \
  V(Int32ConvertInt64, 0xa1, i_l)       \
  V(Int64SConvertFloat32, 0xa2, l_f)    \
  V(Int64SConvertFloat64, 0xa3, l_d)    \
  V(Int64UConvertFloat32, 0xa4, l_f)    \
  V(Int64UConvertFloat64, 0xa5, l_d)    \
  V(Int64SConvertInt32, 0xa6, l_i)      \
  V(Int64UConvertInt32, 0xa7, l_i)      \
  V(Float32SConvertInt32, 0xa8, f_i)    \
  V(Float32UConvertInt32, 0xa9, f_i)    \
  V(Float32SConvertInt64, 0xaa, f_l)    \
  V(Float32UConvertInt64, 0xab, f_l)    \
  V(Float32ConvertFloat64, 0xac, f_d)   \
  V(Float32ReinterpretInt32, 0xad, f_i) \
  V(Float64SConvertInt32, 0xae, d_i)    \
  V(Float64UConvertInt32, 0xaf, d_i)    \
  V(Float64SConvertInt64, 0xb0, d_l)    \
  V(Float64UConvertInt64, 0xb1, d_l)    \
  V(Float64ConvertFloat32, 0xb2, d_f)   \
  V(Float64ReinterpretInt64, 0xb3, f_i)

// All expression opcodes.
#define FOREACH_EXPR_OPCODE(V)     \
  FOREACH_SIMPLE_EXPR_OPCODE(V)    \
  FOREACH_MISC_EXPR_OPCODE(V)      \
  FOREACH_STORE_MEM_EXPR_OPCODE(V) \
  FOREACH_LOAD_MEM_EXPR_OPCODE(V)

// All opcodes.
#define FOREACH_OPCODE(V) \
  FOREACH_STMT_OPCODE(V)  \
  FOREACH_EXPR_OPCODE(V)

// All signatures.
#define FOREACH_SIGNATURE(V)                     \
  V(i_ii, kAstInt32, kAstInt32, kAstInt32)       \
  V(i_i, kAstInt32, kAstInt32)                   \
  V(i_ff, kAstInt32, kAstFloat32, kAstFloat32)   \
  V(i_f, kAstInt32, kAstFloat32)                 \
  V(i_dd, kAstInt32, kAstFloat64, kAstFloat64)   \
  V(i_d, kAstInt32, kAstFloat64)                 \
  V(i_l, kAstInt32, kAstInt64)                   \
  V(l_ll, kAstInt64, kAstInt64, kAstInt64)       \
  V(i_ll, kAstInt32, kAstInt64, kAstInt64)       \
  V(l_l, kAstInt64, kAstInt64)                   \
  V(l_i, kAstInt64, kAstInt32)                   \
  V(l_f, kAstInt64, kAstFloat64)                 \
  V(l_d, kAstInt64, kAstFloat64)                 \
  V(f_ff, kAstFloat32, kAstFloat32, kAstFloat32) \
  V(f_f, kAstFloat32, kAstFloat32)               \
  V(f_d, kAstFloat32, kAstFloat64)               \
  V(f_i, kAstFloat32, kAstInt32)                 \
  V(f_l, kAstFloat32, kAstInt64)                 \
  V(d_dd, kAstFloat64, kAstFloat64, kAstFloat64) \
  V(d_d, kAstFloat64, kAstFloat64)               \
  V(d_f, kAstFloat64, kAstFloat32)               \
  V(d_i, kAstFloat64, kAstInt32)                 \
  V(d_l, kAstFloat64, kAstInt64)                 \
  V(d_id, kAstFloat64, kAstInt32, kAstFloat64)   \
  V(f_if, kAstFloat32, kAstInt32, kAstFloat32)   \
  V(l_il, kAstInt64, kAstInt32, kAstInt64)       \
  V(d_ld, kAstFloat64, kAstInt64, kAstFloat64)   \
  V(f_lf, kAstFloat32, kAstInt64, kAstFloat32)

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

  static byte MemSize(MemType type) {
    switch (type) {
      case kMemInt8:
      case kMemUint8:
        return 1;
      case kMemInt16:
      case kMemUint16:
        return 2;
      case kMemInt32:
      case kMemUint32:
      case kMemFloat32:
        return 4;
      case kMemInt64:
      case kMemUint64:
      case kMemFloat64:
        return 8;
    }
  }

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

  static byte LoadStoreOpcodeOf(MemType type, bool store) {
    switch (type) {
      case kMemInt8:
      case kMemUint8:
      case kMemInt16:
      case kMemUint16:
      case kMemInt32:
      case kMemUint32:
        return store ? kExprInt32StoreMemL : kExprInt32LoadMemL;
      case kMemInt64:
      case kMemUint64:
        return store ? kExprInt64StoreMemL : kExprInt64LoadMemL;
      case kMemFloat32:
        return store ? kExprFloat32StoreMemL : kExprFloat32LoadMemL;
      case kMemFloat64:
        return store ? kExprFloat64StoreMemL : kExprFloat64LoadMemL;
      default:
        UNREACHABLE();
        return kStmtNop;
    }
  }

  static byte LoadStoreAccessOf(MemType type) {
    switch (type) {
      case kMemInt8:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt8) |
            MemoryAccess::SignExtendField::encode(true));
      case kMemUint8:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt8) |
            MemoryAccess::SignExtendField::encode(false));
      case kMemInt16:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt16) |
            MemoryAccess::SignExtendField::encode(true));
      case kMemUint16:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt16) |
            MemoryAccess::SignExtendField::encode(false));
      case kMemInt32:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt32) |
            MemoryAccess::SignExtendField::encode(true));
      case kMemUint32:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt32) |
            MemoryAccess::SignExtendField::encode(false));
      case kMemInt64:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt64) |
            MemoryAccess::SignExtendField::encode(true));
      case kMemUint64:
        return static_cast<byte>(
            MemoryAccess::IntWidthField::encode(MemoryAccess::kInt64) |
            MemoryAccess::SignExtendField::encode(false));
      default:
        return 0;
    }
  }

  static char ShortNameOf(LocalType type) {
    switch (type) {
      case kAstInt32:
        return 'i';
      case kAstInt64:
        return 'l';
      case kAstFloat32:
        return 'f';
      case kAstFloat64:
        return 'd';
      case kAstStmt:
        return 'v';
    }
  }
};
}
}
}

#endif  // V8_WASM_OPCODES_H_
