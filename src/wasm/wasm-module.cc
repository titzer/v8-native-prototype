// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/v8.h"

#include "src/compiler/common-operator.h"
#include "src/compiler/js-graph.h"
#include "src/compiler/machine-operator.h"
#include "src/compiler/pipeline.h"
#include "src/wasm/decoder.h"
#include "src/wasm/tf-builder.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-result.h"

namespace v8 {
namespace internal {
namespace wasm {

std::ostream& operator<<(std::ostream& os, const WasmModule& module) {
  os << "WASM module with ";
  os << (1 << module.mem_size_log2) << " mem bytes";
  if (module.functions) os << module.functions->size() << " functions";
  if (module.globals) os << module.functions->size() << " globals";
  if (module.data_segments) os << module.functions->size() << " data segments";
  return os;
}


// Internal constants for the layout of the module object.
static const int kWasmModuleInternalFieldCount = 4;
static const int kWasmModuleFunctionTable = 0;
static const int kWasmModuleCodeTable = 1;
static const int kWasmMemArrayBuffer = 2;
static const int kWasmGlobalsArrayBuffer = 3;

namespace {
// Helper function to allocate a placeholder code object that will be the
// target of direct calls until the function is compiled.
Handle<Code> CreatePlaceholderCode(Isolate* isolate, uint32_t index) {
  return Handle<Code>::null();  // TODO(titzer)
}


// Helper function to compile a single function.
Handle<Code> CompileFunction(Isolate* isolate, ModuleEnv* module_env,
                             const WasmFunction& function) {
  // Initialize the function environment for decoding.
  FunctionEnv env;
  env.module = module_env;
  env.sig = function.sig;
  env.local_int32_count = function.local_int32_count;
  env.local_int64_count = function.local_int64_count;
  env.local_float32_count = function.local_float32_count;
  env.local_float64_count = function.local_float64_count;

  env.total_locals =                                      // --
      env.local_int32_count + env.local_int64_count +     // --
      env.local_float32_count + env.local_float64_count;  // --

  // Create a TF graph during decoding.
  Zone zone;
  compiler::Graph graph(&zone);
  compiler::CommonOperatorBuilder common(&zone);
  compiler::MachineOperatorBuilder machine(&zone);
  compiler::JSGraph jsgraph(isolate, &graph, &common, nullptr, &machine);
  TreeResult result = BuildTFGraph(
      &jsgraph, &env,                                                 // --
      module_env->module->module_start + function.code_start_offset,  // --
      module_env->module->module_start + function.code_end_offset);   // --

  if (result.failed()) {
    // TODO(titzer): render an appropriate error message and throw.
    return Handle<Code>::null();
  }

  // Run the compiler pipeline to generate machine code.
  compiler::CallDescriptor* descriptor = const_cast<compiler::CallDescriptor*>(
      module_env->GetWasmCallDescriptor(&zone, function.sig));
  return compiler::Pipeline::GenerateCodeForTesting(isolate, descriptor,
                                                    &graph);
}


// Patch the direct calls in a function to other functions.
void PatchDirectCalls(ModuleEnv* module_env, const WasmFunction& function,
                      Handle<Code> code) {
  if (function.external) return;
  // TODO
}


Handle<JSArrayBuffer> NewArrayBuffer(Isolate* isolate, int size,
                                     byte** backing_store) {
  void* memory = isolate->array_buffer_allocator()->Allocate(size);
  if (!memory) return Handle<JSArrayBuffer>::null();
  *backing_store = reinterpret_cast<byte*>(memory);

#if DEBUG
  // Double check the API allocator actually zero-initialized the memory.
  for (uint32_t i = 0; i < size; i++) {
    DCHECK_EQ(0, (*backing_store)[i]);
  }
#endif

  Handle<JSArrayBuffer> buffer = isolate->factory()->NewJSArrayBuffer();
  isolate->heap()->RegisterNewArrayBuffer(isolate->heap()->InNewSpace(*buffer),
                                          memory, size);
  buffer->set_backing_store(memory);
  buffer->set_is_external(false);
  buffer->set_is_neuterable(false);
  buffer->set_byte_length(Smi::FromInt(size));
  return buffer;
}
}


// Instantiates a wasm module as a JSObject.
//  * allocates a backing store of {mem_size} bytes.
//  * installs a named property for that buffer if exported
//  * installs named properties on the object for exported functions
//  * compiles wasm code to machine code
MaybeHandle<JSObject> WasmModule::Instantiate(Isolate* isolate) {
  this->shared_isolate = isolate;  // TODO: have a real shared isolate.

  Factory* factory = isolate->factory();
  // Memory is bigger than maximum supported size.
  if (mem_size_log2 > kMaxMemSize) {
    return isolate->Throw<JSObject>(
        factory->InternalizeUtf8String("Out of memory: wasm memory too large"));
  }

  Handle<Map> map = factory->NewMap(
      JS_OBJECT_TYPE,
      JSObject::kHeaderSize + kWasmModuleInternalFieldCount * kPointerSize);

  //-------------------------------------------------------------------------
  // Allocate the module object.
  //-------------------------------------------------------------------------
  Handle<JSObject> module = factory->NewJSObjectFromMap(map, TENURED);
  Handle<FixedArray> code_table =
      factory->NewFixedArray(static_cast<int>(functions->size()), TENURED);

  //-------------------------------------------------------------------------
  // Allocate the linear memory.
  //-------------------------------------------------------------------------
  uint32_t mem_size = 1 << mem_size_log2;
  byte* mem_addr = nullptr;
  Handle<JSArrayBuffer> mem_buffer =
      NewArrayBuffer(isolate, mem_size, &mem_addr);
  if (!mem_addr) {
    // Not enough space for backing store of mem
    return isolate->Throw<JSObject>(
        factory->InternalizeUtf8String("Out of memory: wasm memory"));
  }

  // Load initialized data segments.
  for (const WasmDataSegment& segment : *data_segments) {
    if (!segment.init) continue;
    CHECK_LT(segment.dest_addr, mem_size);
    CHECK_LT(segment.source_size, mem_size);
    CHECK_LT(segment.dest_addr + segment.source_size, mem_size);
    byte* addr = mem_addr + segment.dest_addr;
    memcpy(addr, module_start + segment.source_offset, segment.source_size);
  }

  module->SetInternalField(kWasmMemArrayBuffer, *mem_buffer);

  if (mem_export) {
    // Export the memory as a named property.
    Handle<String> name = factory->InternalizeUtf8String("memory");
    JSObject::AddProperty(module, name, mem_buffer, READ_ONLY);
  }

  //-------------------------------------------------------------------------
  // Allocate the globals area if necessary.
  //-------------------------------------------------------------------------
  uint32_t globals_size = 0;
  for (const WasmGlobal& global : *globals) {  // maximum of all globals.
    uint32_t end = global.offset + WasmOpcodes::MemSize(global.type);
    if (end > globals_size) globals_size = end;
  }
  byte* globals_addr = nullptr;
  if (globals_size > 0) {
    Handle<JSArrayBuffer> globals_buffer =
        NewArrayBuffer(isolate, mem_size, &globals_addr);
    if (!globals_addr) {
      // Not enough space for backing store of globals.
      return isolate->Throw<JSObject>(
          factory->InternalizeUtf8String("Out of memory: wasm globals"));
    }

    module->SetInternalField(kWasmGlobalsArrayBuffer, *globals_buffer);
  } else {
    module->SetInternalField(kWasmGlobalsArrayBuffer, Smi::FromInt(0));
  }

  //-------------------------------------------------------------------------
  // Compile all functions in the module.
  //-------------------------------------------------------------------------
  int index = 0;
  ModuleEnv module_env;
  module_env.module = this;
  module_env.mem_start = reinterpret_cast<uintptr_t>(mem_addr);
  module_env.mem_end = reinterpret_cast<uintptr_t>(mem_addr) + mem_size;
  module_env.globals_area = reinterpret_cast<uintptr_t>(globals_addr);
  std::vector<Handle<Code>> function_code(functions->size());
  module_env.function_code = &function_code;

  // First pass: compile each function and initialize the code table.
  for (const WasmFunction& func : *functions) {
    Handle<String> name =
        factory->InternalizeUtf8String(GetName(func.name_offset));
    if (func.external) {
      // External functions are read-write properties on this object.
      Handle<Object> undefined = factory->undefined_value();
      JSObject::AddProperty(module, name, undefined, DONT_DELETE);
    } else {
      // Compile the function and install it in the code table.
      Handle<Code> code = CompileFunction(isolate, &module_env, func);
      if (!code.is_null()) {
        function_code[index] = code;
        code_table->set(index, *code);
      }
    }
    if (func.exported) {
      // Export functions are installed as read-only properties on the module.
      Handle<JSFunction> function = factory->NewFunction(name);
      JSObject::AddProperty(module, name, function, READ_ONLY);
      // TODO: create adapter code object for exported functions.
    }
    index++;
  }

  // Second pass: patch all direct call sites.
  for (index = 0; index < functions->size(); index++) {
    PatchDirectCalls(&module_env, functions->at(index), function_code[index]);
  }

  module->SetInternalField(kWasmModuleFunctionTable, Smi::FromInt(0));
  module->SetInternalField(kWasmModuleCodeTable, *code_table);
  return module;
}


Handle<Code> ModuleEnv::GetFunctionCode(uint32_t index) {
  DCHECK(IsValidFunction(index));
  Handle<Code> result = Handle<Code>::null();
  if (function_code) {
    result = function_code->at(index);
    if (result.is_null()) {
      // Allocate placeholder code that will be patched later.
      result = CreatePlaceholderCode(module->shared_isolate, index);
      function_code->at(index) = result;
    }
  }
  return result;
}


const compiler::CallDescriptor* ModuleEnv::GetWasmCallDescriptor(
    Zone* zone, FunctionSig* sig) {
  return nullptr;  // TODO
}


const compiler::CallDescriptor* ModuleEnv::GetCallDescriptor(Zone* zone,
                                                             uint32_t index) {
  return nullptr;  // TODO
}


namespace {
// The main logic for decoding a module.
class ModuleDecoder {
 public:
  ModuleDecoder(Zone* zone, const byte* module_start, const byte* module_end)
      : module_zone(zone),
        start_(module_start),
        cur_(module_start),
        end_(module_end) {
    result_.start = start_;
    if (end_ < start_) {
      error(start_, "end is less than start");
      end_ = start_;
    }
  }

  // Decodes an entire module.
  ModuleResult DecodeModule(WasmModule* module) {
    cur_ = start_;
    result_.val = module;
    module->module_start = start_;
    module->module_end = end_;
    module->mem_size_log2 = 0;
    module->mem_export = false;
    module->mem_external = false;
    module->functions = new std::vector<WasmFunction>();
    module->globals = new std::vector<WasmGlobal>();
    module->data_segments = new std::vector<WasmDataSegment>();

    uint32_t count = u8();
    for (uint32_t i = 0; i < count; i++) {
      if (result_.failed()) break;
      module->functions->push_back({nullptr, 0, 0, 0, 0, 0, 0, false, false});
      DecodeFunction(&module->functions->back(), cur_);
    }

    return result_;
  }

  // Decodes a single function entry.
  void DecodeFunction(WasmFunction* function, const byte* start) {
    cur_ = start;
    function->sig = sig();
    function->name_offset = string();
    function->code_start_offset = offset();
    function->code_end_offset = offset();
    function->local_int32_count = u16();
    function->local_int64_count = u16();
    function->local_float32_count = u16();
    function->local_float64_count = u16();
    function->exported = u8() != 0;
    function->external = u8() != 0;
  }

  FunctionSig* DecodeFunctionSignature(const byte* start) {
    cur_ = start;
    FunctionSig* result = sig();
    return result_.ok() ? result : nullptr;
  }

 private:
  Zone* module_zone;
  const byte* start_;
  const byte* cur_;
  const byte* end_;
  ModuleResult result_;

  // Reads a single 8-bit unsigned integer (byte).
  uint8_t u8() { return read<uint8_t>(); }

  // Reads a single 16-bit unsigned integer (little endian).
  uint16_t u16() {
    return read<uint8_t>() | (static_cast<uint16_t>(read<uint8_t>()) << 8);
  }

  // Reads a single 32-bit unsigned integer interpreted as an offset, checking
  // the offset is within bounds.
  uint32_t offset() {
    uint32_t offset = read<uint32_t>();
    if (offset > (end_ - start_)) {
      error(cur_ - sizeof(uint32_t), "offset out of bounds");
    }
    return offset;
  }

  // Reads a single 32-bit unsigned integer interpreted as an offset into the
  // data and validating the string there.
  uint32_t string() { return offset(); }  // TODO: validate string

  // Reads a single 8-bit integer, interpreting it as a local type.
  LocalType type() {
    byte val = u8();
    LocalType t = static_cast<LocalType>(val);
    switch (t) {
      case kAstStmt:
      case kAstInt32:
      case kAstInt64:
      case kAstFloat32:
      case kAstFloat64:
        return t;
      default:
        error(cur_ - 1, "invalid local type");
        return kAstStmt;
    }
  }

  // Parses an inline function signature.
  FunctionSig* sig() {
    byte count = u8();
    LocalType ret = type();
    FunctionSig::Builder builder(module_zone, ret == kAstStmt ? 0 : 1, count);
    if (ret != kAstStmt) builder.AddReturn(ret);

    for (int i = 0; i < count; i++) {
      // TODO(titzer): disallow void as a parameter type.
      LocalType param = type();
      builder.AddParam(param);
    }
    return builder.Build();
  }

  template <typename T>
  T read() {
    if (cur_ < start_ || cur_ + sizeof(T) > end_) {
      error(cur_, "fell of end of module bytes");
      T val = 0;
      return val;
    }
    T val = *reinterpret_cast<const T*>(cur_);
    cur_ += sizeof(T);
    return val;
  }

  void error(const byte* pc, const char* msg, const byte* pt = nullptr) {
    if (result_.error_code == kSuccess) {
      result_.error_code = kError;  // TODO(titzer): error code
      size_t len = strlen(msg) + 1;
      char* result = new char[len];
      strncpy(result, msg, len);
      result_.error_msg.Reset(result);
      result_.error_pc = pc;
      result_.error_pt = pt;
    }
  }
};
}


ModuleResult DecodeWasmModule(Isolate* isolate, const byte* module_start,
                              const byte* module_end) {
  WasmModule* module = new WasmModule();
  Zone zone;  // TODO(titzer): make the module zone persistent.
  ModuleDecoder decoder(&zone, module_start, module_end);
  return decoder.DecodeModule(module);
}


FunctionSig* DecodeFunctionSignatureForTesting(Zone* zone, const byte* start,
                                               const byte* end) {
  ModuleDecoder decoder(zone, start, end);
  return decoder.DecodeFunctionSignature(start);
}
}
}
}
