// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_RESULT_H_
#define V8_WASM_RESULT_H_

#include "src/v8.h"

#include "src/base/smart-pointers.h"

namespace v8 {
namespace internal {
namespace wasm {

// Error codes for programmatic checking of the decoder's verification.
enum ErrorCode {
  kSuccess,
  kError,                 // TODO(titzer): remove me
  kOutOfMemory,           // decoder ran out of memory
  kEndOfCode,             // end of code reached prematurely
  kInvalidOpcode,         // found invalid opcode
  kUnreachableCode,       // found unreachable code
  kImproperContinue,      // improperly nested continue
  kImproperBreak,         // improperly nested break
  kReturnCount,           // return count mismatch
  kTypeError,             // type mismatch
  kInvalidLocalIndex,     // invalid local
  kInvalidGlobalIndex,    // invalid global
  kInvalidFunctionIndex,  // invalid function
  kInvalidMemType         // invalid memory type
};

// The overall result of decoding a function or a module.
template <typename T>
struct Result {
  Result()
      : val(nullptr), error_code(kSuccess), start(nullptr), error_pc(nullptr) {
    error_msg.Reset(nullptr);
  }

  T val;
  ErrorCode error_code;
  const byte* start;
  const byte* error_pc;
  const byte* error_pt;
  base::SmartArrayPointer<char> error_msg;

  bool ok() const { return error_code == kSuccess; }
  bool failed() const { return error_code != kSuccess; }
};

template <typename T>
std::ostream& operator<<(std::ostream& os, const Result<T>& result) {
  os << "Result = ";
  if (result.ok()) {
    if (result.val != nullptr) {
      os << *result.val;
    } else {
      os << "success (no value)";
    }
  } else if (result.error_msg.get() != nullptr) {
    ptrdiff_t offset = result.error_pc - result.start;
    if (offset < 0) {
      os << result.error_msg.get() << " @" << offset;
    } else {
      os << result.error_msg.get() << " @+" << offset;
    }
  } else {
    os << result.error_code;
  }
  os << std::endl;
  return os;
}


std::ostream& operator<<(std::ostream& os, const ErrorCode& error_code);
}
}
}

#endif
