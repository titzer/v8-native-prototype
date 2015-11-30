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
  kAstStmt  = 0,  // expression that produces no value
  kAstI32   = 1,  // expression that produces an int32 value
  kAstI64   = 2,  // expression that produces an int64 value
  kAstF32   = 3,  // expression that produces a float32 value
  kAstF64   = 4,  // expression that produces a float64 value
  kAstEnd   = 5   // expression that ends control flow
};

// Types for memory accesses and globals.
enum MemType {
  kMemI8 = 0,
  kMemU8 = 1,
  kMemI16 = 2,
  kMemU16 = 3,
  kMemI32 = 4,
  kMemU32 = 5,
  kMemI64 = 6,
  kMemU64 = 7,
  kMemF32 = 8,
  kMemF64 = 9
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

  // Bitfields for the various annotations for memory accesses.
  typedef BitField<Alignment, 7, 1> AlignmentField;
  typedef BitField<Atomicity, 5, 2> AtomicityField;
  typedef BitField<bool, 4, 1> OffsetField;
};

typedef Signature<LocalType> FunctionSig;

// Control expressions and blocks.
#define FOREACH_CONTROL_OPCODE(V) \
  V(Nop,         0x00, _)         \
  V(Block,       0x01, _)         \
  V(Loop,        0x02, _)         \
  V(If,          0x03, _)	  \
  V(IfThen,      0x04, _)         \
  V(Select,      0x05, _)	  \
  V(Br,          0x06, _)         \
  V(BrIf,        0x07, _)	  \
  V(TableSwitch, 0x08, _)         \
  V(Return,      0x14, _)         \
  V(Unreachable, 0x15, _)
// TODO(titzer): numbering

// Constants, locals, globals, and calls.
#define FOREACH_MISC_OPCODE(V) \
  V(I8Const, 0x09, _)          \
  V(I32Const, 0x0a, _)         \
  V(I64Const, 0x0b, _)         \
  V(F64Const, 0x0c, _)         \
  V(F32Const, 0x0d, _)         \
  V(GetLocal, 0x0e, _)         \
  V(SetLocal, 0x0f, _)         \
  V(LoadGlobal, 0x10, _)       \
  V(StoreGlobal, 0x11, _)      \
  V(CallFunction, 0x12, _)     \
  V(CallIndirect, 0x13, _)          

// Load memory expressions.
#define FOREACH_LOAD_MEM_OPCODE(V) \
  V(I32LoadMem8S,  0x20, i_i)          \
  V(I32LoadMem8U,  0x21, i_i)          \
  V(I32LoadMem16S, 0x22, i_i)          \
  V(I32LoadMem16U, 0x23, i_i)          \
  V(I64LoadMem8S,  0x24, l_i)          \
  V(I64LoadMem8U,  0x25, l_i)          \
  V(I64LoadMem16S, 0x26, l_i)          \
  V(I64LoadMem16U, 0x27, l_i)          \
  V(I64LoadMem32S, 0x28, l_i)          \
  V(I64LoadMem32U, 0x29, l_i)          \
  V(I32LoadMem,    0x2a, i_i)          \
  V(I64LoadMem,    0x2b, l_i)          \
  V(F32LoadMem,    0x2c, f_i)          \
  V(F64LoadMem,    0x2d, d_i)

// Store memory expressions.
#define FOREACH_STORE_MEM_OPCODE(V) \
  V(I32StoreMem8,  0x2e, i_ii)           \
  V(I32StoreMem16, 0x2f, i_ii)           \
  V(I64StoreMem8,  0x30, l_il)           \
  V(I64StoreMem16, 0x31, l_il)           \
  V(I64StoreMem32, 0x32, l_il)           \
  V(I32StoreMem,   0x33, i_ii)           \
  V(I64StoreMem,   0x34, l_il)           \
  V(F32StoreMem,   0x35, f_if)           \
  V(F64StoreMem,   0x36, d_id)             

// Load memory expressions.
#define FOREACH_MISC_MEM_OPCODE(V) \
  V(MemorySize, 0x3b, i_v)              \
  V(ResizeMemL, 0x39, i_i)              \
  V(ResizeMemH, 0x3a, l_l)

// Expressions with signatures.
#define FOREACH_SIMPLE_OPCODE(V) \
  V(I32Add, 0x40, i_ii)               \
  V(I32Sub, 0x41, i_ii)               \
  V(I32Mul, 0x42, i_ii)               \
  V(I32DivS, 0x43, i_ii)              \
  V(I32DivU, 0x44, i_ii)              \
  V(I32RemS, 0x45, i_ii)              \
  V(I32RemU, 0x46, i_ii)              \
  V(I32And, 0x47, i_ii)               \
  V(I32Ior, 0x48, i_ii)               \
  V(I32Xor, 0x49, i_ii)               \
  V(I32Shl, 0x4a, i_ii)               \
  V(I32ShrU, 0x4b, i_ii)              \
  V(I32ShrS, 0x4c, i_ii)              \
  V(I32Eq, 0x4d, i_ii)                \
  V(I32Ne, 0x4e, i_ii)                \
  V(I32LtS, 0x4f, i_ii)               \
  V(I32LeS, 0x50, i_ii)               \
  V(I32LtU, 0x51, i_ii)               \
  V(I32LeU, 0x52, i_ii)               \
  V(I32GtS, 0x53, i_ii)               \
  V(I32GeS, 0x54, i_ii)               \
  V(I32GtU, 0x55, i_ii)               \
  V(I32GeU, 0x56, i_ii)               \
  V(I32Clz, 0x57, i_i)                \
  V(I32Ctz, 0x58, i_i)                \
  V(I32Popcnt, 0x59, i_i)             \
  V(BoolNot, 0x5a, i_i)               \
  V(I64Add, 0x5b, l_ll)               \
  V(I64Sub, 0x5c, l_ll)               \
  V(I64Mul, 0x5d, l_ll)               \
  V(I64DivS, 0x5e, l_ll)              \
  V(I64DivU, 0x5f, l_ll)              \
  V(I64RemS, 0x60, l_ll)              \
  V(I64RemU, 0x61, l_ll)              \
  V(I64And, 0x62, l_ll)               \
  V(I64Ior, 0x63, l_ll)               \
  V(I64Xor, 0x64, l_ll)               \
  V(I64Shl, 0x65, l_ll)               \
  V(I64ShrU, 0x66, l_ll)              \
  V(I64ShrS, 0x67, l_ll)              \
  V(I64Eq, 0x68, i_ll)                \
  V(I64Ne, 0x69, i_ll)                \
  V(I64LtS, 0x6a, i_ll)               \
  V(I64LeS, 0x6b, i_ll)               \
  V(I64LtU, 0x6c, i_ll)               \
  V(I64LeU, 0x6d, i_ll)               \
  V(I64GtS, 0x6e, i_ll)               \
  V(I64GeS, 0x6f, i_ll)               \
  V(I64GtU, 0x70, i_ll)               \
  V(I64GeU, 0x71, i_ll)               \
  V(I64Clz, 0x72, l_l)                \
  V(I64Ctz, 0x73, l_l)                \
  V(I64Popcnt, 0x74, l_l)             \
  V(F32Add, 0x75, f_ff)               \
  V(F32Sub, 0x76, f_ff)               \
  V(F32Mul, 0x77, f_ff)               \
  V(F32Div, 0x78, f_ff)               \
  V(F32Min, 0x79, f_ff)               \
  V(F32Max, 0x7a, f_ff)               \
  V(F32Abs, 0x7b, f_f)                \
  V(F32Neg, 0x7c, f_f)                \
  V(F32CopySign, 0x7d, f_ff)          \
  V(F32Ceil, 0x7e, f_f)               \
  V(F32Floor, 0x7f, f_f)              \
  V(F32Trunc, 0x80, f_f)              \
  V(F32NearestInt, 0x81, f_f)         \
  V(F32Sqrt, 0x82, f_f)               \
  V(F32Eq, 0x83, i_ff)                \
  V(F32Ne, 0x84, i_ff)                \
  V(F32Lt, 0x85, i_ff)                \
  V(F32Le, 0x86, i_ff)                \
  V(F32Gt, 0x87, i_ff)                \
  V(F32Ge, 0x88, i_ff)                \
  V(F64Add, 0x89, d_dd)               \
  V(F64Sub, 0x8a, d_dd)               \
  V(F64Mul, 0x8b, d_dd)               \
  V(F64Div, 0x8c, d_dd)               \
  V(F64Min, 0x8d, d_dd)               \
  V(F64Max, 0x8e, d_dd)               \
  V(F64Abs, 0x8f, d_d)                \
  V(F64Neg, 0x90, d_d)                \
  V(F64CopySign, 0x91, d_dd)          \
  V(F64Ceil, 0x92, d_d)               \
  V(F64Floor, 0x93, d_d)              \
  V(F64Trunc, 0x94, d_d)              \
  V(F64NearestInt, 0x95, d_d)         \
  V(F64Sqrt, 0x96, d_d)               \
  V(F64Eq, 0x97, i_dd)                \
  V(F64Ne, 0x98, i_dd)                \
  V(F64Lt, 0x99, i_dd)                \
  V(F64Le, 0x9a, i_dd)                \
  V(F64Gt, 0x9b, i_dd)                \
  V(F64Ge, 0x9c, i_dd)                \
  V(I32SConvertF32, 0x9d, i_f)        \
  V(I32SConvertF64, 0x9e, i_d)        \
  V(I32UConvertF32, 0x9f, i_f)        \
  V(I32UConvertF64, 0xa0, i_d)        \
  V(I32ConvertI64, 0xa1, i_l)         \
  V(I64SConvertF32, 0xa2, l_f)        \
  V(I64SConvertF64, 0xa3, l_d)        \
  V(I64UConvertF32, 0xa4, l_f)        \
  V(I64UConvertF64, 0xa5, l_d)        \
  V(I64SConvertI32, 0xa6, l_i)        \
  V(I64UConvertI32, 0xa7, l_i)        \
  V(F32SConvertI32, 0xa8, f_i)        \
  V(F32UConvertI32, 0xa9, f_i)        \
  V(F32SConvertI64, 0xaa, f_l)        \
  V(F32UConvertI64, 0xab, f_l)        \
  V(F32ConvertF64, 0xac, f_d)         \
  V(F32ReinterpretI32, 0xad, f_i)     \
  V(F64SConvertI32, 0xae, d_i)        \
  V(F64UConvertI32, 0xaf, d_i)        \
  V(F64SConvertI64, 0xb0, d_l)        \
  V(F64UConvertI64, 0xb1, d_l)        \
  V(F64ConvertF32, 0xb2, d_f)         \
  V(F64ReinterpretI64, 0xb3, d_l)     \
  V(I32ReinterpretF32, 0xb4, i_f)     \
  V(I64ReinterpretF64, 0xb5, l_d)

// All opcodes.
#define FOREACH_OPCODE(V)     \
  FOREACH_CONTROL_OPCODE(V)        \
  FOREACH_MISC_OPCODE(V)      \
  FOREACH_SIMPLE_OPCODE(V)    \
  FOREACH_STORE_MEM_OPCODE(V) \
  FOREACH_LOAD_MEM_OPCODE(V)  \
  FOREACH_MISC_MEM_OPCODE(V)

// All signatures.
#define FOREACH_SIGNATURE(V)         \
  V(i_ii, kAstI32, kAstI32, kAstI32) \
  V(i_i, kAstI32, kAstI32)           \
  V(i_v, kAstI32)                    \
  V(i_ff, kAstI32, kAstF32, kAstF32) \
  V(i_f, kAstI32, kAstF32)           \
  V(i_dd, kAstI32, kAstF64, kAstF64) \
  V(i_d, kAstI32, kAstF64)           \
  V(i_l, kAstI32, kAstI64)           \
  V(l_ll, kAstI64, kAstI64, kAstI64) \
  V(i_ll, kAstI32, kAstI64, kAstI64) \
  V(l_l, kAstI64, kAstI64)           \
  V(l_i, kAstI64, kAstI32)           \
  V(l_f, kAstI64, kAstF32)           \
  V(l_d, kAstI64, kAstF64)           \
  V(f_ff, kAstF32, kAstF32, kAstF32) \
  V(f_f, kAstF32, kAstF32)           \
  V(f_d, kAstF32, kAstF64)           \
  V(f_i, kAstF32, kAstI32)           \
  V(f_l, kAstF32, kAstI64)           \
  V(d_dd, kAstF64, kAstF64, kAstF64) \
  V(d_d, kAstF64, kAstF64)           \
  V(d_f, kAstF64, kAstF32)           \
  V(d_i, kAstF64, kAstI32)           \
  V(d_l, kAstF64, kAstI64)           \
  V(d_id, kAstF64, kAstI32, kAstF64) \
  V(f_if, kAstF32, kAstI32, kAstF32) \
  V(l_il, kAstI64, kAstI32, kAstI64) 

enum WasmOpcode {
// Declare expression opcodes.
#define DECLARE_NAMED_ENUM(name, opcode, sig) kExpr##name = opcode,
      FOREACH_OPCODE(DECLARE_NAMED_ENUM)
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
      case kMemI8:
      case kMemU8:
        return 1;
      case kMemI16:
      case kMemU16:
        return 2;
      case kMemI32:
      case kMemU32:
      case kMemF32:
        return 4;
      case kMemI64:
      case kMemU64:
      case kMemF64:
        return 8;
    }
  }

  static LocalType LocalTypeFor(MemType type) {
    switch (type) {
      case kMemI8:
      case kMemU8:
      case kMemI16:
      case kMemU16:
      case kMemI32:
      case kMemU32:
        return kAstI32;
      case kMemI64:
      case kMemU64:
        return kAstI64;
      case kMemF32:
        return kAstF32;
      case kMemF64:
        return kAstF64;
    }
  }

  static byte LoadStoreOpcodeOf(MemType type, bool store) {
    switch (type) {
      case kMemI8:
        return store ? kExprI32StoreMem8 : kExprI32LoadMem8S;
      case kMemU8:
        return store ? kExprI32StoreMem8 : kExprI32LoadMem8U;
      case kMemI16:
        return store ? kExprI32StoreMem16 : kExprI32LoadMem16S;
      case kMemU16:
        return store ? kExprI32StoreMem16 : kExprI32LoadMem16U;
      case kMemI32:
      case kMemU32:
        return store ? kExprI32StoreMem : kExprI32LoadMem;
      case kMemI64:
      case kMemU64:
        return store ? kExprI64StoreMem : kExprI64LoadMem;
      case kMemF32:
        return store ? kExprF32StoreMem : kExprF32LoadMem;
      case kMemF64:
        return store ? kExprF64StoreMem : kExprF64LoadMem;
      default:
        UNREACHABLE();
        return kExprNop;
    }
  }

  static byte LoadStoreAccessOf(MemType type, bool with_offset = false) {
    return MemoryAccess::OffsetField::encode(with_offset);
  }

  static char ShortNameOf(LocalType type) {
    switch (type) {
      case kAstI32:
        return 'i';
      case kAstI64:
        return 'l';
      case kAstF32:
        return 'f';
      case kAstF64:
        return 'd';
      case kAstStmt:
        return 'v';
      case kAstEnd:
        return 'x';
    }
  }
};
}
}
}

#endif  // V8_WASM_OPCODES_H_
