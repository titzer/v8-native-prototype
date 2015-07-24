// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-result.h"

namespace v8 {
namespace internal {
namespace wasm {

std::ostream& operator<<(std::ostream& os, const ErrorCode& error_code) {
  switch (error_code) {
    case kSuccess:
      os << "Success";
      break;
    default:  // TODO(titzer): render error codes
      os << "Error";
      break;
  }
  return os;
}
}
}
}
