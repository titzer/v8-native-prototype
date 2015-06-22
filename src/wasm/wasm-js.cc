// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-js.h"

namespace v8 {

void WasmJs::Install(Isolate* isolate, Handle<ObjectTemplate> global_template) {
  // Bind the WASM object.
  Handle<ObjectTemplate> wasm_template = ObjectTemplate::New(isolate);
  wasm_template->Set(String::NewFromUtf8(isolate, "verify",
                                         NewStringType::kNormal).ToLocalChecked(),
                     FunctionTemplate::New(isolate, Verify));
  global_template->Set(String::NewFromUtf8(isolate, "WASM",
                                           NewStringType::kNormal).ToLocalChecked(),
                       wasm_template);
}


void WasmJs::Verify(const v8::FunctionCallbackInfo<v8::Value>& args) {
  printf("Unimplemented.\n");
}

}
