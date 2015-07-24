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

void WasmJs::Install(Isolate* isolate, Local<ObjectTemplate> global_template) {
  // Bind the WASM object.
  Local<ObjectTemplate> wasm_template = ObjectTemplate::New(isolate);
  wasm_template->Set(
      String::NewFromUtf8(isolate, "verifyModule", NewStringType::kNormal)
          .ToLocalChecked(),
      FunctionTemplate::New(isolate, VerifyModule));
  global_template->Set(
      String::NewFromUtf8(isolate, "WASM", NewStringType::kNormal)
          .ToLocalChecked(),
      wasm_template);
}


void WasmJs::VerifyModule(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  if (args.Length() < 1 || !args[0]->IsArrayBuffer()) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(),
                            "WASM.verify(): Argument 1 must be an array buffer",
                            NewStringType::kNormal).ToLocalChecked());
    return;
  }
  Local<ArrayBuffer> buffer = Local<ArrayBuffer>::Cast(args[0]);
  ArrayBuffer::Contents contents = buffer->Externalize();

  if (contents.ByteLength() < kMinModuleSize) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(
            args.GetIsolate(),
            "WASM.verify(): ArrayBuffer smaller than minimum module size",
            NewStringType::kNormal).ToLocalChecked());
  }

  if (contents.ByteLength() >= kMaxModuleSize) {
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(
            args.GetIsolate(),
            "WASM.verify(): ArrayBuffer larger than maximum module size",
            NewStringType::kNormal).ToLocalChecked());
  }

  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
  const byte* module_start = reinterpret_cast<const byte*>(contents.Data());
  const byte* module_end = module_start + contents.ByteLength();

  internal::wasm::ModuleResult result =
      internal::wasm::DecodeWasmModule(isolate, module_start, module_end);

  if (result.failed()) {
    std::ostringstream str;
    str << "WASM.verify() failed: " << result;
    args.GetIsolate()->ThrowException(
        String::NewFromUtf8(args.GetIsolate(), str.str().c_str(),
                            NewStringType::kNormal).ToLocalChecked());
  }
}
}
