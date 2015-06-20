// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WEBASM_MACRO_GEN_H_
#define V8_WEBASM_MACRO_GEN_H_

#include "src/wasm/wasm-opcodes.h"

// Convenience macros for building Wasm bytecode directly into a byte array.

//------------------------------------------------------------------------------
// Statements.
//------------------------------------------------------------------------------
#define WASM_SET_LOCAL(index, val) kStmtSetLocal, static_cast<byte>(index), val
#define WASM_SET_GLOBAL(index, val) \
  kStmtSetGlobal, static_cast<byte>(index), val
#define WASM_SET_HEAP(type, index, val) \
  kStmtSetHeap, static_cast<byte>(type), index, val
#define WASM_IF(cond, tstmt) kStmtIf, cond, tstmt
#define WASM_IF_THEN(cond, tstmt, fstmt) kStmtIfThen, cond, tstmt, fstmt
#define WASM_NOP kStmtNop
#define WASM_BLOCK(count, ...) kStmtBlock, static_cast<byte>(count), __VA_ARGS__
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
#define WASM_FLOAT32(val)                                       \
  kExprFloat32Const, static_cast<byte>(bit_cast<int32_t>(val)), \
      static_cast<byte>(bit_cast<uint32_t>(val) >> 8),          \
      static_cast<byte>(bit_cast<uint32_t>(val) >> 16),         \
      static_cast<byte>(bit_cast<uint32_t>(val) >> 24)
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
#define WASM_GET_GLOBAL(index) kExprGetGlobal, static_cast<byte>(index)
#define WASM_GET_HEAP(type, index) kExprGetHeap, static_cast<byte>(type), index
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
#define WASM_WHILE(x, y) \
  kStmtLoop, 2, kStmtIf, kExprBoolNot, x, kStmtBreak, 0, y
#define WASM_INC_LOCAL(index)                                       \
  kStmtSetLocal, static_cast<byte>(index), kExprInt32Add, kInt8, 1, \
      kExprGetLocal, static_cast<byte>(index)
#define WASM_INC_LOCAL_BY(index, count)                          \
  kStmtSetLocal, static_cast<byte>(index), kExprInt32Add, kInt8, \
      static_cast<int8_t>(count), kExprGetLocal, static_cast<byte>(index)


#define WASM_BINOP(opcode, x, y) static_cast<byte>(opcode), x, y

//------------------------------------------------------------------------------
// Integer binops.
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
#define WASM_INT32_SLT(x, y) kExprInt32Slt, x, y
#define WASM_INT32_SLE(x, y) kExprInt32Sle, x, y
#define WASM_INT32_ULT(x, y) kExprInt32Ult, x, y
#define WASM_INT32_ULE(x, y) kExprInt32Ule, x, y

//------------------------------------------------------------------------------
// Float64 binops.
//------------------------------------------------------------------------------
#define WASM_FLOAT64_ADD(x, y) kExprFloat64Add, x, y
#define WASM_FLOAT64_SUB(x, y) kExprFloat64Sub, x, y
#define WASM_FLOAT64_MUL(x, y) kExprFloat64Mul, x, y
#define WASM_FLOAT64_DIV(x, y) kExprFloat64Div, x, y
#define WASM_FLOAT64_EQ(x, y) kExprFloat64Eq, x, y
#define WASM_FLOAT64_LT(x, y) kExprFloat64Lt, x, y
#define WASM_FLOAT64_LE(x, y) kExprFloat64Le, x, y


//------------------------------------------------------------------------------
// Float32 binops.
//------------------------------------------------------------------------------
#define WASM_FLOAT32_ADD(x, y) kExprFloat32Add, x, y
#define WASM_FLOAT32_SUB(x, y) kExprFloat32Sub, x, y
#define WASM_FLOAT32_MUL(x, y) kExprFloat32Mul, x, y
#define WASM_FLOAT32_DIV(x, y) kExprFloat32Div, x, y
#define WASM_FLOAT32_EQ(x, y) kExprFloat32Eq, x, y
#define WASM_FLOAT32_LT(x, y) kExprFloat32Lt, x, y
#define WASM_FLOAT32_LE(x, y) kExprFloat32Le, x, y


//------------------------------------------------------------------------------
// Type conversions.
//------------------------------------------------------------------------------
#define WASM_INT32_FROM_FLOAT32(x) kExprInt32FromFloat32, x
#define WASM_INT32_FROM_FLOAT64(x) kExprInt32FromFloat64, x
#define WASM_UINT32_FROM_FLOAT32(x) kExprUint32FromFloat32, x
#define WASM_UINT32_FROM_FLOAT64(x) kExprUint32FromFloat64, x
#define WASM_FLOAT64_FROM_SINT32(x) kExprFloat64FromSInt32, x
#define WASM_FLOAT64_FROM_UINT32(x) kExprFloat64FromUInt32, x
#define WASM_FLOAT64_FROM_FLOAT32(x) kExprFloat64FromFloat32, x
#define WASM_FLOAT32_FROM_SINT32(x) kExprFloat32FromSInt32, x
#define WASM_FLOAT32_FROM_UINT32(x) kExprFloat32FromUInt32, x
#define WASM_FLOAT32_FROM_FLOAT64(x) kExprFloat32FromFloat64, x

#endif  // V8_WEBASM_MACRO_GEN_H_
