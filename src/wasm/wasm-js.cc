// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-js.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-result.h"

typedef uint8_t byte;

namespace v8 {

const size_t kMinModuleSize = 8;
const size_t kMaxModuleSize = 1024 * 1024 * 1024;
const size_t kMaxFunctionSize = 128 * 1024;

void WasmJs::Install(Isolate* isolate, Local<ObjectTemplate> global_template) {
  // Bind the WASM object.
  Local<ObjectTemplate> wasm_template = ObjectTemplate::New(isolate);
  wasm_template->Set(
      String::NewFromUtf8(isolate, "verifyModule", NewStringType::kNormal)
          .ToLocalChecked(),
      FunctionTemplate::New(isolate, VerifyModule));
  wasm_template->Set(
      String::NewFromUtf8(isolate, "verifyFunction", NewStringType::kNormal)
          .ToLocalChecked(),
      FunctionTemplate::New(isolate, VerifyFunction));
  global_template->Set(
      String::NewFromUtf8(isolate, "WASM", NewStringType::kNormal)
          .ToLocalChecked(),
      wasm_template);
}

namespace {
struct RawBuffer {
  const byte* start;
  const byte* end;
  size_t size() { return static_cast<size_t>(end - start); }
};

RawBuffer GetRawBufferArgument(
    const char* call, const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1 || !args[0]->IsArrayBuffer()) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(),
                            "Argument 0 must be an array buffer",
                            NewStringType::kNormal).ToLocalChecked());
    return {nullptr, nullptr};
  }
  Local<ArrayBuffer> buffer = Local<ArrayBuffer>::Cast(args[0]);
  ArrayBuffer::Contents contents = buffer->Externalize();

  // TODO(titzer): allow offsets into buffers, views, etc.

  const byte* start = reinterpret_cast<const byte*>(contents.Data());
  const byte* end = start + contents.ByteLength();
  return {start, end};
}
}

void WasmJs::VerifyModule(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());

  RawBuffer buffer = GetRawBufferArgument("WASM.verifyModule()", args);

  if (buffer.size() < kMinModuleSize) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(
            args.GetIsolate(),
            "WASM.verifyModule(): ArrayBuffer smaller than minimum module size",
            NewStringType::kNormal).ToLocalChecked());
  }

  if (buffer.size() > kMaxModuleSize) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(
            args.GetIsolate(),
            "WASM.verifyModule(): ArrayBuffer larger than maximum module size",
            NewStringType::kNormal).ToLocalChecked());
  }

  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());

  internal::wasm::ModuleResult result =
      internal::wasm::DecodeWasmModule(isolate, buffer.start, buffer.end);

  if (result.failed()) {
    std::ostringstream str;
    str << "WASM.verifyModule() failed: " << result;
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(), str.str().c_str(),
                            NewStringType::kNormal).ToLocalChecked());
  }
}


void WasmJs::VerifyFunction(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());

  // TODO(titzer): no need to externalize to get the bytes for verification.
  RawBuffer buffer = GetRawBufferArgument("WASM.verifyFunction()", args);

  if (buffer.size() > kMaxFunctionSize) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(),
                            "WASM.verifyFunction(): ArrayBuffer larger than "
                            "maximum function size",
                            NewStringType::kNormal).ToLocalChecked());
  }

  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());

  internal::wasm::FunctionResult result;
  {
    // Verification of a single function shouldn't allocate.
    i::DisallowHeapAllocation no_allocation;
    i::Zone zone;
    result = internal::wasm::DecodeWasmFunction(isolate, &zone, nullptr,
                                                buffer.start, buffer.end);
  }

  if (result.failed()) {
    std::ostringstream str;
    str << "WASM.verifyFunction() failed: " << result;
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(), str.str().c_str(),
                            NewStringType::kNormal).ToLocalChecked());
  }
}
}
