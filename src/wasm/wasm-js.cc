// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/api-natives.h"
#include "src/api.h"
#include "src/assert-scope.h"
#include "src/ast.h"
#include "src/factory.h"
#include "src/handles.h"
#include "src/isolate.h"
#include "src/objects.h"
#include "src/parser.h"
#include "src/scopes.h"

#include "src/wasm/asm-wasm-builder.h"
#include "src/wasm/wasm-js.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-result.h"

typedef uint8_t byte;

using v8::internal::wasm::ErrorThrower;

namespace v8 {

namespace {
struct RawBuffer {
  const byte* start;
  const byte* end;
  size_t size() { return static_cast<size_t>(end - start); }
};

RawBuffer GetRawBufferArgument(
    ErrorThrower& thrower, const v8::FunctionCallbackInfo<v8::Value>& args) {
  if (args.Length() < 1 || !args[0]->IsArrayBuffer()) {
    thrower.Error("Argument 0 must be an array buffer");
    return {nullptr, nullptr};
  }
  Local<ArrayBuffer> buffer = Local<ArrayBuffer>::Cast(args[0]);
  ArrayBuffer::Contents contents =
      buffer->IsExternal() ? buffer->GetContents() : buffer->Externalize();

  // TODO(titzer): allow offsets into buffers, views, etc.

  const byte* start = reinterpret_cast<const byte*>(contents.Data());
  const byte* end = start + contents.ByteLength();

  if (start == nullptr) {
    thrower.Error("ArrayBuffer argument is empty");
  }
  return {start, end};
}

void VerifyModule(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
  ErrorThrower thrower(isolate, "WASM.verifyModule()");

  RawBuffer buffer = GetRawBufferArgument(thrower, args);
  if (thrower.error()) return;

  i::Zone zone;
  internal::wasm::ModuleResult result = internal::wasm::DecodeWasmModule(
      isolate, &zone, buffer.start, buffer.end);

  if (result.failed()) {
    thrower.Failed("", result);
  }

  if (result.val) delete result.val;
}


void VerifyFunction(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
  ErrorThrower thrower(isolate, "WASM.verifyFunction()");

  // TODO(titzer): no need to externalize to get the bytes for verification.
  RawBuffer buffer = GetRawBufferArgument(thrower, args);
  if (thrower.error()) return;

  internal::wasm::FunctionResult result;
  {
    // Verification of a single function shouldn't allocate.
    i::DisallowHeapAllocation no_allocation;
    i::Zone zone;
    result = internal::wasm::DecodeWasmFunction(isolate, &zone, nullptr,
                                                buffer.start, buffer.end);
  }

  if (result.failed()) {
    thrower.Failed("", result);
  }

  if (result.val) delete result.val;
}


void CompileRun(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
  ErrorThrower thrower(isolate, "WASM.compileRun()");

  RawBuffer buffer = GetRawBufferArgument(thrower, args);
  if (thrower.error()) return;

  // Decode and pre-verify the functions before compiling and running.
  i::Zone zone;
  internal::wasm::ModuleResult result = internal::wasm::DecodeWasmModule(
      isolate, &zone, buffer.start, buffer.end, true);

  if (result.failed()) {
    thrower.Failed("", result);
  } else {
    // Success. Compile and run!
    int32_t retval = i::wasm::CompileAndRunWasmModule(isolate, result.val);
    args.GetReturnValue().Set(retval);
  }

  if (result.val) delete result.val;
}


void AsmCompileRun(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
  ErrorThrower thrower(isolate, "WASM.asmCompileRun()");

  if (args.Length() != 1) {
    thrower.Error("Invalid argument count");
  }
  if (!args[0]->IsString()) {
    thrower.Error("Invalid argument count");
  }

  i::Factory* factory = isolate->factory();
  i::Zone zone;
  Local<String> source = Local<String>::Cast(args[0]);
  i::Handle<i::Script> script = factory->NewScript(
      Utils::OpenHandle(*source));

  i::ParseInfo info(&zone, script);
  i::Parser parser(&info);
  parser.set_allow_harmony_arrow_functions(true);
  parser.set_allow_harmony_sloppy(true);
  info.set_global();
  info.set_lazy(false);
  info.set_allow_lazy_parsing(false);
  info.set_toplevel(true);

  i::CompilationInfo compilation_info(&info);
  CHECK(i::Compiler::ParseAndAnalyze(&info));
  info.set_literal(
      info.scope()->declarations()->at(0)->AsFunctionDeclaration()->fun());

  v8::internal::wasm::AsmWasmBuilder(&compilation_info).Run();
  args.GetReturnValue().Set(0);
}


void InstantiateModule(const v8::FunctionCallbackInfo<v8::Value>& args) {
  HandleScope scope(args.GetIsolate());
  i::Isolate* isolate = reinterpret_cast<i::Isolate*>(args.GetIsolate());
  ErrorThrower thrower(isolate, "WASM.instantiateModule()");

  RawBuffer buffer = GetRawBufferArgument(thrower, args);
  if (buffer.start == nullptr) return;

  // Decode but avoid a redundant pass over function bodies for verification.
  // Verification will happen during compilation.
  i::Zone zone;
  internal::wasm::ModuleResult result = internal::wasm::DecodeWasmModule(
      isolate, &zone, buffer.start, buffer.end, false);

  if (result.failed()) {
    thrower.Failed("", result);
  } else {
    // Success. Instantiate the module and return the object.
    i::Handle<i::JSObject> ffi = i::Handle<i::JSObject>::null();
    if (args.Length() > 1 && args[1]->IsObject()) {
      Local<Object> obj = Local<Object>::Cast(args[1]);
      ffi = v8::Utils::OpenHandle(*obj);
    }

    i::MaybeHandle<i::JSObject> object = result.val->Instantiate(isolate, ffi);

    if (!object.is_null()) {
      args.GetReturnValue().Set(v8::Utils::ToLocal(object.ToHandleChecked()));
    }
  }

  if (result.val) delete result.val;
}
}


// TODO(titzer): we use the API to create the function template because the
// internal guts are too ugly to replicate here.
static i::Handle<i::FunctionTemplateInfo> NewTemplate(i::Isolate* i_isolate,
                                                      FunctionCallback func) {
  Isolate* isolate = reinterpret_cast<Isolate*>(i_isolate);
  Local<FunctionTemplate> local = FunctionTemplate::New(isolate, func);
  return v8::Utils::OpenHandle(*local);
}


namespace internal {
static Handle<String> v8_str(Isolate* isolate, const char* str) {
  return isolate->factory()->NewStringFromAsciiChecked(str);
}


static void InstallFunc(Isolate* isolate, Handle<JSObject> object,
                        const char* str, FunctionCallback func) {
  Handle<String> name = v8_str(isolate, str);
  Handle<FunctionTemplateInfo> temp = NewTemplate(isolate, func);
  Handle<JSFunction> function =
      ApiNatives::InstantiateFunction(temp).ToHandleChecked();
  PropertyAttributes attributes =
      static_cast<PropertyAttributes>(DONT_DELETE | READ_ONLY);
  JSObject::AddProperty(object, name, function, attributes);
}


void WasmJs::Install(Isolate* isolate, Handle<JSGlobalObject> global) {
  // Bind the WASM object.
  Factory* factory = isolate->factory();
  Handle<String> name = v8_str(isolate, "WASM");
  Handle<JSFunction> cons = factory->NewFunction(name);
  JSFunction::SetInstancePrototype(
      cons, Handle<Object>(global->native_context()->initial_object_prototype(),
                           isolate));
  cons->SetInstanceClassName(*name);
  Handle<JSObject> wasm_object = factory->NewJSObject(cons, TENURED);
  PropertyAttributes attributes = static_cast<PropertyAttributes>(DONT_ENUM);
  JSObject::AddProperty(global, name, wasm_object, attributes);

  // Install functions on the WASM object.
  InstallFunc(isolate, wasm_object, "instantiateModule", InstantiateModule);
  InstallFunc(isolate, wasm_object, "verifyModule", VerifyModule);
  InstallFunc(isolate, wasm_object, "verifyFunction", VerifyFunction);
  InstallFunc(isolate, wasm_object, "compileRun", CompileRun);
  InstallFunc(isolate, wasm_object, "asmCompileRun", AsmCompileRun);
}
}  // namespace internal
}  // namespace v8
