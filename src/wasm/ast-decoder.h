// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_DECODER_H_
#define V8_WASM_DECODER_H_

#include "src/signature.h"
#include "src/zone-containers.h"

#include "src/base/smart-pointers.h"

#include "src/wasm/wasm-opcodes.h"
#include "src/wasm/wasm-result.h"

#include "src/flags.h"

namespace v8 {
namespace internal {

namespace compiler {  // external declarations from compiler.
class WasmGraphBuilder;
}

namespace wasm {

typedef compiler::WasmGraphBuilder TFBuilder;
struct ModuleEnv;  // forward declaration of module interface.

// Interface the function environment during decoding, include the signature
// and number of locals.
struct FunctionEnv {
  ModuleEnv* module;             // module environment
  FunctionSig* sig;              // signature of this function
  uint32_t local_int32_count;    // number of int32 locals
  uint32_t local_int64_count;    // number of int64 locals
  uint32_t local_float32_count;  // number of float32 locals
  uint32_t local_float64_count;  // number of float64 locals
  uint32_t total_locals;         // sum of parameters and all locals

  bool IsValidLocal(uint32_t index) { return index < total_locals; }
  uint32_t GetLocalCount() { return total_locals; }
  LocalType GetLocalType(uint32_t index) {
    if (index < sig->parameter_count()) return sig->GetParam(index);
    index -= sig->parameter_count();
    if (index < local_int32_count) return kAstI32;
    index -= local_int32_count;
    if (index < local_int64_count) return kAstI64;
    index -= local_int64_count;
    if (index < local_float32_count) return kAstF32;
    index -= local_float32_count;
    if (index < local_float64_count) return kAstF64;
    return kAstStmt;
  }

  void AddLocals(LocalType type, uint32_t count) {
    switch (type) {
      case kAstI32:
        local_int32_count += count;
        break;
      case kAstI64:
        local_int64_count += count;
        break;
      case kAstF32:
        local_float32_count += count;
        break;
      case kAstF64:
        local_float64_count += count;
        break;
      default:
        UNREACHABLE();
    }
    total_locals += count;
    DCHECK(total_locals ==
           (sig->parameter_count() + local_int32_count + local_int64_count +
            local_float32_count + local_float64_count));
  }

  void SumLocals() {
    total_locals = static_cast<uint32_t>(sig->parameter_count()) +
                   local_int32_count + local_int64_count + local_float32_count +
                   local_float64_count;
  }
};

struct Tree;
typedef Result<Tree*> TreeResult;

std::ostream& operator<<(std::ostream& os, const Tree& tree);

TreeResult VerifyWasmCode(FunctionEnv* env, const byte* base, const byte* start,
                          const byte* end);
TreeResult BuildTFGraph(TFBuilder* builder, FunctionEnv* env, const byte* base,
                        const byte* start, const byte* end);

inline TreeResult VerifyWasmCode(FunctionEnv* env, const byte* start,
                                 const byte* end) {
  return VerifyWasmCode(env, nullptr, start, end);
}

inline TreeResult BuildTFGraph(TFBuilder* builder, FunctionEnv* env,
                               const byte* start, const byte* end) {
  return BuildTFGraph(builder, env, nullptr, start, end);
}

enum ReadUnsignedLEB128ErrorCode { kNoError, kInvalidLEB128, kMissingLEB128 };

ReadUnsignedLEB128ErrorCode ReadUnsignedLEB128Operand(const byte*, const byte*,
                                                      int*, uint32_t*);

// Computes the length of the opcode at the given address.
int OpcodeLength(const byte* pc);

// Computes the arity (number of sub-nodes) of the opcode at the given address.
int OpcodeArity(FunctionEnv* env, const byte* pc);

#if DEBUG
#define TRACE(...)                                    \
  do {                                                \
    if (FLAG_trace_wasm_decoder) PrintF(__VA_ARGS__); \
  } while (false)
#else
#define TRACE(...)
#endif

// A helper utility to decode bytes, integers, fields, varints, etc, from
// a buffer of bytes.
class Decoder {
 public:
  Decoder(const byte* start, const byte* end)
      : start_(start),
        pc_(start),
        limit_(end),
        error_pc_(nullptr),
        error_pt_(nullptr) {}

  virtual ~Decoder() {}

  // Reads a 8-bit unsigned integer (byte) and advances {pc_}.
  uint8_t u8(const char* name = nullptr) {
    TRACE("  +%d  %-20s: ", static_cast<int>(pc_ - start_),
          name ? name : "uint8_t");
    if (checkAvailable(1)) {
      byte val = *(pc_++);
      TRACE("%02x = %d\n", val, val);
      return val;
    } else {
      error("expected 1 byte, but fell off end");
      return traceOffEnd<uint8_t>();
    }
  }

  // Reads a 16-bit unsigned integer (little endian) and advances {pc_}.
  uint16_t u16(const char* name = nullptr) {
    TRACE("  +%d  %-20s: ", static_cast<int>(pc_ - start_),
          name ? name : "uint16_t");
    if (checkAvailable(2)) {
#ifdef V8_TARGET_LITTLE_ENDIAN
      byte b0 = pc_[0];
      byte b1 = pc_[1];
#else
      byte b1 = pc_[0];
      byte b0 = pc_[1];
#endif
      uint16_t val = static_cast<uint16_t>(b1 << 8) | b0;
      TRACE("%02x %02x = %d\n", pc_[0], pc_[1], val);
      pc_ += 2;
      return val;
    } else {
      error("expected 2 bytes, but fell off end");
      return traceOffEnd<uint16_t>();
    }
  }

  // Reads a single 32-bit unsigned integer (little endian) and advances {pc_}.
  uint32_t u32(const char* name = nullptr) {
    TRACE("  +%d  %-20s: ", static_cast<int>(pc_ - start_),
          name ? name : "uint32_t");
    if (checkAvailable(4)) {
#ifdef V8_TARGET_LITTLE_ENDIAN
      byte b0 = pc_[0];
      byte b1 = pc_[1];
      byte b2 = pc_[2];
      byte b3 = pc_[3];
#else
      byte b3 = pc_[0];
      byte b2 = pc_[1];
      byte b1 = pc_[2];
      byte b0 = pc_[3];
#endif
      uint32_t val = static_cast<uint32_t>(b3 << 24) |
                     static_cast<uint32_t>(b2 << 16) |
                     static_cast<uint32_t>(b1 << 8) | b0;
      TRACE("%02x %02x %02x %02x = %u\n", pc_[0], pc_[1], pc_[2], pc_[3], val);
      pc_ += 4;
      return val;
    } else {
      error("expected 4 bytes, but fell off end");
      return traceOffEnd<uint32_t>();
    }
  }

  // Reads a LEB128 variable-length 32-bit integer and advances {pc_}.
  uint32_t u32v(int* length, const char* name = nullptr) {
    TRACE("  +%d  %-20s: ", static_cast<int>(pc_ - start_),
          name ? name : "varint");

    if (!checkAvailable(1)) {
      error("expected at least 1 byte, but fell off end");
      return traceOffEnd<uint32_t>();
    }

    const byte* pos = pc_;
    const byte* end = pc_ + 5;
    if (end > limit_) end = limit_;

    uint32_t result = 0;
    int shift = 0;
    byte b = 0;
    while (pc_ < end) {
      b = *pc_++;
      TRACE("%02x ", b);
      result = result | ((b & 0x7F) << shift);
      if ((b & 0x80) == 0) break;
      shift += 7;
    }

    *length = static_cast<int>(pc_ - pos);
    if (pc_ == end && (b & 0x80)) {
      error(pc_ - 1, "varint too large");
    } else {
      TRACE("= %u\n", result);
    }
    return result;
  }

  // Check that at least {size} bytes exist between {pc_} and {limit_}.
  bool checkAvailable(int size) {
    if (pc_ < start_ || (pc_ + size) > limit_) {
      error(pc_, nullptr, "expected %d bytes, fell off end", size);
      return false;
    } else {
      return true;
    }
  }

  void error(const char* msg) { error(pc_, nullptr, msg); }

  void error(const byte* pc, const char* msg) { error(pc, nullptr, msg); }

  // Sets internal error state.
  void error(const byte* pc, const byte* pt, const char* format, ...) {
    if (ok()) {
#if DEBUG
      if (FLAG_wasm_break_on_decoder_error) {
        base::OS::DebugBreak();
      }
#endif
      const int kMaxErrorMsg = 256;
      char* buffer = new char[kMaxErrorMsg];
      va_list arguments;
      va_start(arguments, format);
      base::OS::VSNPrintF(buffer, kMaxErrorMsg - 1, format, arguments);
      va_end(arguments);
      error_msg_.Reset(buffer);
      error_pc_ = pc;
      error_pt_ = pt;
      onFirstError();
    }
  }

  // Behavior triggered on first error, overridden in subclasses.
  virtual void onFirstError() {}

  // Debugging helper to print bytes up to the end.
  template <typename T>
  T traceOffEnd() {
    T t = 0;
    for (const byte* ptr = pc_; ptr < limit_; ptr++) {
      TRACE("%02x ", *ptr);
    }
    TRACE("<end>\n");
    pc_ = limit_;
    return t;
  }

  // Converts the given value to a {Result}, copying the error if necessary.
  template <typename T>
  Result<T> toResult(T val) {
    Result<T> result;
    if (error_pc_) {
      result.error_code = kError;
      result.start = start_;
      result.error_pc = error_pc_;
      result.error_pt = error_pt_;
      result.error_msg = error_msg_;
      error_msg_.Reset(nullptr);
    } else {
      result.error_code = kSuccess;
      result.val = val;
    }
    return result;
  }

  // Resets the boundaries of this decoder.
  void Reset(const byte* start, const byte* end) {
    start_ = start;
    pc_ = start;
    limit_ = end;
    error_pc_ = nullptr;
    error_pt_ = nullptr;
    error_msg_.Reset(nullptr);
  }

  bool ok() const { return error_pc_ == nullptr; }
  bool failed() const { return error_pc_ != nullptr; }

 protected:
  const byte* start_;
  const byte* pc_;
  const byte* limit_;
  const byte* error_pc_;
  const byte* error_pt_;
  base::SmartArrayPointer<char> error_msg_;
};

#undef TRACE
}
}
}

#endif  // V8_WASM_DECODER_H_
