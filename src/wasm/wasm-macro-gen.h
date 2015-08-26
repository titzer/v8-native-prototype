// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_MACRO_GEN_H_
#define V8_WASM_MACRO_GEN_H_

#include "src/wasm/wasm-opcodes.h"

// Convenience macros for building Wasm bytecode directly into a byte array.

//------------------------------------------------------------------------------
// Statements.
//------------------------------------------------------------------------------
#define WASM_IF(cond, tstmt) kStmtIf, cond, tstmt
#define WASM_IF_THEN(cond, tstmt, fstmt) kStmtIfThen, cond, tstmt, fstmt
#define WASM_NOP kStmtNop
#define WASM_BLOCK(count, ...) kStmtBlock, static_cast<byte>(count), __VA_ARGS__
#define WASM_INFINITE_LOOP kStmtLoop, 0
#define WASM_LOOP(count, ...) kStmtLoop, static_cast<byte>(count), __VA_ARGS__
#define WASM_SWITCH(count, key, ...) \
  kStmtSwitch, static_cast<byte>(count), key, __VA_ARGS__
#define WASM_SWITCH_NF(count, key, ...) \
  kStmtSwitchNf, static_cast<byte>(count), key, __VA_ARGS__
#define WASM_CONTINUE(depth) kStmtContinue, static_cast<byte>(depth)
#define WASM_BREAK(depth) kStmtBreak, static_cast<byte>(depth)
#define WASM_RETURN0 kStmtReturn
#define WASM_RETURN(...) kStmtReturn, __VA_ARGS__


//------------------------------------------------------------------------------
// Misc expressions.
//------------------------------------------------------------------------------
#define WASM_ID(...) __VA_ARGS__
#define WASM_ZERO kExprInt8Const, 0
#define WASM_ONE kExprInt8Const, 1
#define WASM_INT8(val) kExprInt8Const, static_cast<byte>(val)
#define WASM_INT32(val)                                                 \
  kExprInt32Const, static_cast<byte>(val), static_cast<byte>(val >> 8), \
      static_cast<byte>(val >> 16), static_cast<byte>(val >> 24)
#define WASM_INT64(val)                                           \
  kExprInt64Const, static_cast<byte>(static_cast<uint64_t>(val)), \
      static_cast<byte>(static_cast<uint64_t>(val) >> 8),         \
      static_cast<byte>(static_cast<uint64_t>(val) >> 16),        \
      static_cast<byte>(static_cast<uint64_t>(val) >> 24),        \
      static_cast<byte>(static_cast<uint64_t>(val) >> 32),        \
      static_cast<byte>(static_cast<uint64_t>(val) >> 40),        \
      static_cast<byte>(static_cast<uint64_t>(val) >> 48),        \
      static_cast<byte>(static_cast<uint64_t>(val) >> 56)
#define WASM_FLOAT32(val)                                                   \
  kExprFloat32Const,                                                        \
      static_cast<byte>(bit_cast<int32_t>(static_cast<float>(val))),        \
      static_cast<byte>(bit_cast<uint32_t>(static_cast<float>(val)) >> 8),  \
      static_cast<byte>(bit_cast<uint32_t>(static_cast<float>(val)) >> 16), \
      static_cast<byte>(bit_cast<uint32_t>(static_cast<float>(val)) >> 24)
#define WASM_FLOAT64(val)                                        \
  kExprFloat64Const, static_cast<byte>(bit_cast<uint64_t>(val)), \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 8),           \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 16),          \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 24),          \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 32),          \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 40),          \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 48),          \
      static_cast<byte>(bit_cast<uint64_t>(val) >> 56)
#define WASM_GET_LOCAL(index) kExprGetLocal, static_cast<byte>(index)
#define WASM_SET_LOCAL(index, val) kExprSetLocal, static_cast<byte>(index), val
#define WASM_LOAD_GLOBAL(index) kExprLoadGlobal, static_cast<byte>(index)
#define WASM_STORE_GLOBAL(index, val) \
  kExprStoreGlobal, static_cast<byte>(index), val
#define WASM_LOAD_MEM(type, index)                                 \
  v8::internal::wasm::WasmOpcodes::LoadStoreOpcodeOf(type, false), \
      v8::internal::wasm::WasmOpcodes::LoadStoreAccessOf(type), index
#define WASM_STORE_MEM(type, index, val)                          \
  v8::internal::wasm::WasmOpcodes::LoadStoreOpcodeOf(type, true), \
      v8::internal::wasm::WasmOpcodes::LoadStoreAccessOf(type), index, val
#define WASM_CALL_FUNCTION(index, ...) \
  kExprCallFunction, static_cast<byte>(index), __VA_ARGS__
#define WASM_CALL_INDIRECT(index, func, ...) \
  kExprCallIndirect, static_cast<byte>(index), func, __VA_ARGS__
#define WASM_CALL_FUNCTION0(index) kExprCallFunction, static_cast<byte>(index)
#define WASM_CALL_INDIRECT0(index, func) \
  kExprCallIndirect, static_cast<byte>(index), func
#define WASM_TERNARY(cond, tval, fval) kExprTernary, cond, tval, fval
#define WASM_COMMA(left, right) kExprComma, left, right
#define WASM_NOT(x) kExprBoolNot, x


//------------------------------------------------------------------------------
// Statements and expressions that are composed of multiple bytecodes.
//------------------------------------------------------------------------------
#define WASM_WHILE(x, y) kStmtLoop, 1, kStmtIfThen, x, y, kStmtBreak, 0
#define WASM_INC_LOCAL(index)                                            \
  kExprSetLocal, static_cast<byte>(index), kExprInt32Add, kExprGetLocal, \
      static_cast<byte>(index), kExprInt8Const, 1
#define WASM_INC_LOCAL_BY(index, count)                                  \
  kExprSetLocal, static_cast<byte>(index), kExprInt32Add, kExprGetLocal, \
      static_cast<byte>(index), kExprInt8Const, static_cast<int8_t>(count)


#define WASM_UNOP(opcode, x) static_cast<byte>(opcode), x
#define WASM_BINOP(opcode, x, y) static_cast<byte>(opcode), x, y

//------------------------------------------------------------------------------
// Int32 operations
//------------------------------------------------------------------------------
#define WASM_INT32_ADD(x, y) kExprInt32Add, x, y
#define WASM_INT32_SUB(x, y) kExprInt32Sub, x, y
#define WASM_INT32_MUL(x, y) kExprInt32Mul, x, y
#define WASM_INT32_SDIV(x, y) kExprInt32SDiv, x, y
#define WASM_INT32_UDIV(x, y) kExprInt32UDiv, x, y
#define WASM_INT32_SREM(x, y) kExprInt32SRem, x, y
#define WASM_INT32_UREM(x, y) kExprInt32URem, x, y
#define WASM_INT32_AND(x, y) kExprInt32And, x, y
#define WASM_INT32_IOR(x, y) kExprInt32Ior, x, y
#define WASM_INT32_XOR(x, y) kExprInt32Xor, x, y
#define WASM_INT32_SHL(x, y) kExprInt32Shl, x, y
#define WASM_INT32_SHR(x, y) kExprInt32Shr, x, y
#define WASM_INT32_SAR(x, y) kExprInt32Sar, x, y
#define WASM_INT32_EQ(x, y) kExprInt32Eq, x, y
#define WASM_INT32_NE(x, y) kExprInt32Ne, x, y
#define WASM_INT32_SLT(x, y) kExprInt32Slt, x, y
#define WASM_INT32_SLE(x, y) kExprInt32Sle, x, y
#define WASM_INT32_ULT(x, y) kExprInt32Ult, x, y
#define WASM_INT32_ULE(x, y) kExprInt32Ule, x, y
#define WASM_INT32_SGT(x, y) kExprInt32Sgt, x, y
#define WASM_INT32_SGE(x, y) kExprInt32Sge, x, y
#define WASM_INT32_UGT(x, y) kExprInt32Ugt, x, y
#define WASM_INT32_UGE(x, y) kExprInt32Uge, x, y
#define WASM_INT32_CLZ(x) kExprInt32Clz, x
#define WASM_INT32_CTZ(x) kExprInt32Ctz, x
#define WASM_INT32_POPCNT(x) kExprInt32PopCnt, x

//------------------------------------------------------------------------------
// Int64 operations
//------------------------------------------------------------------------------
#define WASM_INT64_ADD(x, y) kExprInt64Add, x, y
#define WASM_INT64_SUB(x, y) kExprInt64Sub, x, y
#define WASM_INT64_MUL(x, y) kExprInt64Mul, x, y
#define WASM_INT64_SDIV(x, y) kExprInt64SDiv, x, y
#define WASM_INT64_UDIV(x, y) kExprInt64UDiv, x, y
#define WASM_INT64_SREM(x, y) kExprInt64SRem, x, y
#define WASM_INT64_UREM(x, y) kExprInt64URem, x, y
#define WASM_INT64_AND(x, y) kExprInt64And, x, y
#define WASM_INT64_IOR(x, y) kExprInt64Ior, x, y
#define WASM_INT64_XOR(x, y) kExprInt64Xor, x, y
#define WASM_INT64_SHL(x, y) kExprInt64Shl, x, y
#define WASM_INT64_SHR(x, y) kExprInt64Shr, x, y
#define WASM_INT64_SAR(x, y) kExprInt64Sar, x, y
#define WASM_INT64_EQ(x, y) kExprInt64Eq, x, y
#define WASM_INT64_NE(x, y) kExprInt64Ne, x, y
#define WASM_INT64_SLT(x, y) kExprInt64Slt, x, y
#define WASM_INT64_SLE(x, y) kExprInt64Sle, x, y
#define WASM_INT64_ULT(x, y) kExprInt64Ult, x, y
#define WASM_INT64_ULE(x, y) kExprInt64Ule, x, y
#define WASM_INT64_SGT(x, y) kExprInt64Sgt, x, y
#define WASM_INT64_SGE(x, y) kExprInt64Sge, x, y
#define WASM_INT64_UGT(x, y) kExprInt64Ugt, x, y
#define WASM_INT64_UGE(x, y) kExprInt64Uge, x, y
#define WASM_INT64_CLZ(x) kExprInt64Clz, x
#define WASM_INT64_CTZ(x) kExprInt64Ctz, x
#define WASM_INT64_POPCNT(x) kExprInt64PopCnt, x

//------------------------------------------------------------------------------
// Float32 operations
//------------------------------------------------------------------------------
#define WASM_FLOAT32_ADD(x, y) kExprFloat32Add, x, y
#define WASM_FLOAT32_SUB(x, y) kExprFloat32Sub, x, y
#define WASM_FLOAT32_MUL(x, y) kExprFloat32Mul, x, y
#define WASM_FLOAT32_DIV(x, y) kExprFloat32Div, x, y
#define WASM_FLOAT32_MIN(x, y) kExprFloat32Min, x, y
#define WASM_FLOAT32_MAX(x, y) kExprFloat32Max, x, y
#define WASM_FLOAT32_ABS(x) kExprFloat32Abs, x
#define WASM_FLOAT32_NEG(x) kExprFloat32Neg, x
#define WASM_FLOAT32_COPYSIGN(x) kExprFloat32CopySign, x
#define WASM_FLOAT32_CEIL(x) kExprFloat32Ceil, x
#define WASM_FLOAT32_FLOOR(x) kExprFloat32Floor, x
#define WASM_FLOAT32_TRUNC(x) kExprFloat32Trunc, x
#define WASM_FLOAT32_NEARESTINT(x) kExprFloat32NearestInt, x
#define WASM_FLOAT32_SQRT(x) kExprFloat32Sqrt, x
#define WASM_FLOAT32_EQ(x, y) kExprFloat32Eq, x, y
#define WASM_FLOAT32_NE(x, y) kExprFloat32Ne, x, y
#define WASM_FLOAT32_LT(x, y) kExprFloat32Lt, x, y
#define WASM_FLOAT32_LE(x, y) kExprFloat32Le, x, y
#define WASM_FLOAT32_GT(x, y) kExprFloat32Gt, x, y
#define WASM_FLOAT32_GE(x, y) kExprFloat32Ge, x, y


//------------------------------------------------------------------------------
// Float64 operations
//------------------------------------------------------------------------------
#define WASM_FLOAT64_ADD(x, y) kExprFloat64Add, x, y
#define WASM_FLOAT64_SUB(x, y) kExprFloat64Sub, x, y
#define WASM_FLOAT64_MUL(x, y) kExprFloat64Mul, x, y
#define WASM_FLOAT64_DIV(x, y) kExprFloat64Div, x, y
#define WASM_FLOAT64_MIN(x, y) kExprFloat64Min, x, y
#define WASM_FLOAT64_MAX(x, y) kExprFloat64Max, x, y
#define WASM_FLOAT64_ABS(x) kExprFloat64Abs, x
#define WASM_FLOAT64_NEG(x) kExprFloat64Neg, x
#define WASM_FLOAT64_COPYSIGN(x) kExprFloat64CopySign, x
#define WASM_FLOAT64_CEIL(x) kExprFloat64Ceil, x
#define WASM_FLOAT64_FLOOR(x) kExprFloat64Floor, x
#define WASM_FLOAT64_TRUNC(x) kExprFloat64Trunc, x
#define WASM_FLOAT64_NEARESTINT(x) kExprFloat64NearestInt, x
#define WASM_FLOAT64_SQRT(x) kExprFloat64Sqrt, x
#define WASM_FLOAT64_EQ(x, y) kExprFloat64Eq, x, y
#define WASM_FLOAT64_NE(x, y) kExprFloat64Ne, x, y
#define WASM_FLOAT64_LT(x, y) kExprFloat64Lt, x, y
#define WASM_FLOAT64_LE(x, y) kExprFloat64Le, x, y
#define WASM_FLOAT64_GT(x, y) kExprFloat64Gt, x, y
#define WASM_FLOAT64_GE(x, y) kExprFloat64Ge, x, y


//------------------------------------------------------------------------------
// Type conversions.
//------------------------------------------------------------------------------
#define WASM_INT32_SCONVERT_FLOAT32(x) kExprInt32SConvertFloat32, x
#define WASM_INT32_SCONVERT_FLOAT64(x) kExprInt32SConvertFloat64, x
#define WASM_INT32_UCONVERT_FLOAT32(x) kExprInt32UConvertFloat32, x
#define WASM_INT32_UCONVERT_FLOAT64(x) kExprInt32UConvertFloat64, x
#define WASM_INT32_CONVERT_INT64(x) kExprInt32ConvertInt64, x
#define WASM_INT64_SCONVERT_FLOAT32(x) kExprInt64SConvertFloat32, x
#define WASM_INT64_SCONVERT_FLOAT64(x) kExprInt64SConvertFloat64, x
#define WASM_INT64_UCONVERT_FLOAT32(x) kExprInt64UConvertFloat32, x
#define WASM_INT64_UCONVERT_FLOAT64(x) kExprInt64UConvertFloat64, x
#define WASM_INT64_SCONVERT_INT32(x) kExprInt64SConvertInt32, x
#define WASM_INT64_UCONVERT_INT32(x) kExprInt64UConvertInt32, x
#define WASM_FLOAT32_SCONVERT_INT32(x) kExprFloat32SConvertInt32, x
#define WASM_FLOAT32_UCONVERT_INT32(x) kExprFloat32UConvertInt32, x
#define WASM_FLOAT32_SCONVERT_INT64(x) kExprFloat32SConvertInt64, x
#define WASM_FLOAT32_UCONVERT_INT64(x) kExprFloat32UConvertInt64, x
#define WASM_FLOAT32_CONVERT_FLOAT64(x) kExprFloat32ConvertFloat64, x
#define WASM_FLOAT32_REINTERPRET_INT32(x) kExprFloat32ReinterpretInt32, x
#define WASM_FLOAT64_SCONVERT_INT32(x) kExprFloat64SConvertInt32, x
#define WASM_FLOAT64_UCONVERT_INT32(x) kExprFloat64UConvertInt32, x
#define WASM_FLOAT64_SCONVERT_INT64(x) kExprFloat64SConvertInt64, x
#define WASM_FLOAT64_UCONVERT_INT64(x) kExprFloat64UConvertInt64, x
#define WASM_FLOAT64_CONVERT_FLOAT32(x) kExprFloat64ConvertFloat32, x
#define WASM_FLOAT64_REINTERPRET_INT64(x) kExprFloat64ReinterpretInt64, x

#endif  // V8_WASM_MACRO_GEN_H_
