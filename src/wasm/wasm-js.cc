// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-js.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-result.h"

typedef uint8_t byte;

namespace v8 {

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
  ArrayBuffer::Contents contents =
      buffer->IsExternal() ? buffer->GetContents() : buffer->Externalize();

  // TODO(titzer): allow offsets into buffers, views, etc.

  const byte* start = reinterpret_cast<const byte*>(contents.Data());
  const byte* end = start + contents.ByteLength();
  return {start, end};
}

void VerifyModule(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  RawBuffer buffer = GetRawBufferArgument("WASM.verifyModule()", args);
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());

  i::Zone zone;
  internal::wasm::ModuleResult result = internal::wasm::DecodeWasmModule(
      isolate, &zone, buffer.start, buffer.end);

  if (result.failed()) {
    std::ostringstream str;
    str << "WASM.verifyModule() failed: " << result;
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(), str.str().c_str(),
                            NewStringType::kNormal).ToLocalChecked());
  }
  // TODO(titzer): free memory created by DecodeWasmModule.
}


void VerifyFunction(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  // TODO(titzer): no need to externalize to get the bytes for verification.
  RawBuffer buffer = GetRawBufferArgument("WASM.verifyFunction()", args);
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
  // TODO(titzer): free memory created by DecodeWasmFunction.
}


void CompileRun(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  RawBuffer buffer = GetRawBufferArgument("WASM.compileRun()", args);
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());

  i::Zone zone;
  internal::wasm::ModuleResult result = internal::wasm::DecodeWasmModule(
      isolate, &zone, buffer.start, buffer.end);

  if (result.failed()) {
    std::ostringstream str;
    str << "WASM.compileRun() failed: " << result;
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(), str.str().c_str(),
                            NewStringType::kNormal).ToLocalChecked());
  }

  int32_t retval =
      i::wasm::CompileAndRunWasmModule(isolate, buffer.start, buffer.end);
  args.GetReturnValue().Set(retval);
}
}


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
  wasm_template->Set(
      String::NewFromUtf8(isolate, "compileRun", NewStringType::kNormal)
          .ToLocalChecked(),
      FunctionTemplate::New(isolate, CompileRun));
  global_template->Set(
      String::NewFromUtf8(isolate, "WASM", NewStringType::kNormal)
          .ToLocalChecked(),
      wasm_template);
}
}
