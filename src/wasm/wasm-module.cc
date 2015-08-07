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


std::ostream& operator<<(std::ostream& os, const WasmFunction& function) {
  os << "WASM function with signature ";

  // TODO(titzer): factor out rendering of signatures.
  if (function.sig->return_count() == 0) os << "v";
  for (size_t i = 0; i < function.sig->return_count(); i++) {
    os << WasmOpcodes::ShortNameOf(function.sig->GetReturn(i));
  }
  os << "_";
  if (function.sig->parameter_count() == 0) os << "v";
  for (size_t i = 0; i < function.sig->parameter_count(); i++) {
    os << WasmOpcodes::ShortNameOf(function.sig->GetParam(i));
  }
  os << " locals: ";
  if (function.local_int32_count)
    os << function.local_int32_count << " int32s ";
  if (function.local_int64_count)
    os << function.local_int64_count << " int64s ";
  if (function.local_float32_count)
    os << function.local_float32_count << " float32s ";
  if (function.local_float64_count)
    os << function.local_float64_count << " float64s ";

  os << " code bytes: "
     << (function.code_end_offset - function.code_start_offset);
  return os;
}


namespace {
// Internal constants for the layout of the module object.
const int kWasmModuleInternalFieldCount = 4;
const int kWasmModuleFunctionTable = 0;
const int kWasmModuleCodeTable = 1;
const int kWasmMemArrayBuffer = 2;
const int kWasmGlobalsArrayBuffer = 3;

// Helper function to allocate a placeholder code object that will be the
// target of direct calls until the function is compiled.
Handle<Code> CreatePlaceholderCode(Isolate* isolate, uint32_t index) {
  return Handle<Code>::null();  // TODO(titzer)
}


// Helper function to compile a single function.
Handle<Code> CompileFunction(Isolate* isolate, ModuleEnv* module_env,
                             const WasmFunction& function, int index) {
  // Initialize the function environment for decoding.
  FunctionEnv env;
  env.module = module_env;
  env.sig = function.sig;
  env.local_int32_count = function.local_int32_count;
  env.local_int64_count = function.local_int64_count;
  env.local_float32_count = function.local_float32_count;
  env.local_float64_count = function.local_float64_count;
  env.SumLocals();

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
  Handle<Code> code =
      compiler::Pipeline::GenerateCodeForTesting(isolate, descriptor, &graph);

#ifdef ENABLE_DISASSEMBLER
  // Disassemble the code for debugging.
  if (!code.is_null() && FLAG_print_opt_code) {
    static const int kBufferSize = 128;
    char buffer[kBufferSize];
    const char* name = "";
    if (function.name_offset > 0) {
      const byte* ptr = module_env->module->module_start + function.name_offset;
      name = reinterpret_cast<const char*>(ptr);
    }
    snprintf(buffer, kBufferSize, "WASM function #%d:%s", index, name);
    OFStream os(stdout);
    code->Disassemble(buffer, os);
  }
#endif
  return code;
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

// The main logic for decoding the bytes of a module.
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
  ModuleResult DecodeModule(WasmModule* module, bool verify_functions = true) {
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

    // Decode the module header.
    uint32_t globals_count = u16();        // read number of globals
    uint32_t functions_count = u16();      // read number of functions
    uint32_t data_segments_count = u16();  // read number of data segments

    // Decode globals.
    for (uint32_t i = 0; i < globals_count; i++) {
      if (result_.failed()) break;
      module->globals->push_back({0, kMemInt32, 0, false});
      WasmGlobal* global = &module->globals->back();
      DecodeGlobalInModule(global);
    }

    // Set up module environment for verification.
    ModuleEnv menv;
    menv.module = module;
    menv.globals_area = 0;
    menv.mem_start = 0;
    menv.mem_end = 0;
    menv.function_code = nullptr;

    // Decode functions.
    for (uint32_t i = 0; i < functions_count; i++) {
      if (result_.failed()) break;
      module->functions->push_back({nullptr, 0, 0, 0, 0, 0, 0, false, false});
      WasmFunction* function = &module->functions->back();
      DecodeFunctionInModule(function, verify_functions);

      if (result_.ok() && verify_functions) {
        if (!function->external) VerifyFunctionBody(i, &menv, function);
      }
    }

    // Decode data segments.
    for (uint32_t i = 0; i < data_segments_count; i++) {
      if (result_.failed()) break;
      module->data_segments->push_back({0, 0, 0});
      WasmDataSegment* segment = &module->data_segments->back();
      DecodeDataSegmentInModule(segment);
    }

    return result_;
  }

  // Decodes a single anonymous function starting at {start_}.
  FunctionResult DecodeSingleFunction(ModuleEnv* module_env,
                                      WasmFunction* function) {
    cur_ = start_;
    function->sig = sig();                        // read signature
    function->name_offset = 0;                    // ---- name
    function->code_start_offset = off(cur_ + 8);  // ---- code start
    function->code_end_offset = off(end_);        // ---- code end
    function->local_int32_count = u16();          // read u16
    function->local_int64_count = u16();          // read u16
    function->local_float32_count = u16();        // read u16
    function->local_float64_count = u16();        // read u16
    function->exported = false;                   // ---- exported
    function->external = false;                   // ---- external

    if (result_.ok()) {
      VerifyFunctionBody(0, module_env, function);
    }

    FunctionResult result;
    // Copy error code and location.
    result.CopyFrom(result_);
    result.val = function;
    return result;
  }

  // Decodes a single function signature at {start}.
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

  uint32_t off(const byte* ptr) { return static_cast<uint32_t>(ptr - start_); }

  // Decodes a single global entry inside a module starting at {cur_}.
  void DecodeGlobalInModule(WasmGlobal* global) {
    global->name_offset = string();  // read global name
    global->type = mem_type();       // read global memory type
    global->offset = 0;              // ---- offset is computed later
    global->exported = u8() != 0;    // read exported flag
  }

  // Decodes a single function entry inside a module starting at {cur_}.
  void DecodeFunctionInModule(WasmFunction* function, bool verify_body = true) {
    function->sig = sig();                   // read function signature
    function->name_offset = string();        // read function name
    function->code_start_offset = offset();  // read code start offset
    function->code_end_offset = offset();    // read code end offset
    function->local_int32_count = u16();     // read local int32 count
    function->local_int64_count = u16();     // read local int64 count
    function->local_float32_count = u16();   // read local float32 count
    function->local_float64_count = u16();   // read local float64 count
    function->exported = u8() != 0;          // read exported flag
    function->external = u8() != 0;          // read external flag
  }

  // Decodes a single data segment entry inside a module starting at {cur_}.
  void DecodeDataSegmentInModule(WasmDataSegment* segment) {
    segment->dest_addr = u32();  // TODO: check it's within the memory size.
    segment->source_offset = offset();
    segment->source_size = u32();  // TODO: check the size is reasonable.
    segment->init = u8();
  }

  // Verifies the body (code) of a given function.
  void VerifyFunctionBody(uint32_t func_num, ModuleEnv* menv,
                          WasmFunction* function) {
    FunctionEnv fenv;
    fenv.module = menv;
    fenv.sig = function->sig;
    fenv.local_int32_count = function->local_int32_count;
    fenv.local_int64_count = function->local_int64_count;
    fenv.local_float32_count = function->local_float32_count;
    fenv.local_float64_count = function->local_float64_count;
    fenv.SumLocals();

    TreeResult result =
        VerifyWasmCode(&fenv, start_ + function->code_start_offset,
                       start_ + function->code_end_offset);
    if (result.failed()) {
      // Wrap the error message from the function decoder.
      std::ostringstream str;
      str << "in function #" << func_num << ": ";
      // TODO(titzer): add function name for the user?
      str << result;
      const char* raw = str.str().c_str();
      size_t len = strlen(raw);
      char* buffer = new char[len];
      strncpy(buffer, raw, len);

      // Copy error code and location.
      result_.CopyFrom(result);
      result_.error_msg.Reset(buffer);
    }
  }

  // Reads a single 8-bit unsigned integer (byte).
  uint8_t u8() { return read<uint8_t>(); }

  // Reads a single 16-bit unsigned integer (little endian).
  uint16_t u16() { return read<uint16_t>(); }

  // Reads a single 32-bit unsigned integer (little endian).
  uint32_t u32() { return read<uint32_t>(); }

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
  LocalType local_type() {
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

  // Reads a single 8-bit integer, interpreting it as a memory type.
  MemType mem_type() {
    byte val = u8();
    MemType t = static_cast<MemType>(val);
    switch (t) {
      case kMemInt8:
      case kMemUint8:
      case kMemInt16:
      case kMemUint16:
      case kMemInt32:
      case kMemUint32:
      case kMemInt64:
      case kMemUint64:
      case kMemFloat32:
      case kMemFloat64:
        return t;
      default:
        error(cur_ - 1, "invalid memory type");
        return kMemInt32;
    }
  }

  // Parses an inline function signature.
  FunctionSig* sig() {
    byte count = u8();
    LocalType ret = local_type();
    FunctionSig::Builder builder(module_zone, ret == kAstStmt ? 0 : 1, count);
    if (ret != kAstStmt) builder.AddReturn(ret);

    for (int i = 0; i < count; i++) {
      LocalType param = local_type();
      if (param == kAstStmt) error(cur_ - 1, "invalid void parameter type");
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
#if DEBUG
    if (FLAG_wasm_break_on_decoder_error) {
      base::OS::DebugBreak();
    }
#endif
    if (result_.error_code == kSuccess) {
      result_.error_code = kError;  // TODO(titzer): better error code
      size_t len = strlen(msg) + 1;
      char* result = new char[len];
      strncpy(result, msg, len);
      result_.error_msg.Reset(result);
      result_.error_pc = pc;
      result_.error_pt = pt;
    }
  }
};


size_t AllocateGlobalsOffsets(std::vector<WasmGlobal>* globals) {
  uint32_t offset = 0;
  if (!globals) return 0;
  for (WasmGlobal& global : *globals) {
    byte size = WasmOpcodes::MemSize(global.type);
    offset = (offset + size - 1) & ~(size - 1);  // align
    global.offset = offset;
    offset += size;
  }
  return offset;
}


size_t ComputeGlobalsSize(std::vector<WasmGlobal>* globals) {
  uint32_t globals_size = 0;
  if (!globals) return 0;
  for (const WasmGlobal& global : *globals) {  // maximum of all globals.
    uint32_t end = global.offset + WasmOpcodes::MemSize(global.type);
    if (end > globals_size) globals_size = end;
  }
  return globals_size;
}

void LoadDataSegments(WasmModule* module, byte* mem_addr, size_t mem_size) {
  for (const WasmDataSegment& segment : *module->data_segments) {
    if (!segment.init) continue;
    CHECK_LT(segment.dest_addr, mem_size);
    CHECK_LT(segment.source_size, mem_size);
    CHECK_LT(segment.dest_addr + segment.source_size, mem_size);
    byte* addr = mem_addr + segment.dest_addr;
    memcpy(addr, module->module_start + segment.source_offset,
           segment.source_size);
  }
}
}  // namespace


// Instantiates a wasm module as a JSObject.
//  * allocates a backing store of {mem_size} bytes.
//  * installs a named property "memory" for that buffer if exported
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
  LoadDataSegments(this, mem_addr, mem_size);

  module->SetInternalField(kWasmMemArrayBuffer, *mem_buffer);

  if (mem_export) {
    // Export the memory as a named property.
    Handle<String> name = factory->InternalizeUtf8String("memory");
    JSObject::AddProperty(module, name, mem_buffer, READ_ONLY);
  }

  //-------------------------------------------------------------------------
  // Allocate the globals area if necessary.
  //-------------------------------------------------------------------------
  size_t globals_size = ComputeGlobalsSize(globals);
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
      Handle<Code> code = CompileFunction(isolate, &module_env, func, index);
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


compiler::CallDescriptor* ModuleEnv::GetCallDescriptor(Zone* zone,
                                                       uint32_t index) {
  DCHECK(IsValidFunction(index));
  WasmFunction* function = &module->functions->at(index);
  if (!function->external) {
    // WASM -> WASM direct call.
    return GetWasmCallDescriptor(zone, function->sig);
  }
  return nullptr;  // TODO
}


// Helpers for nice error messages.
class ModuleError : public ModuleResult {
 public:
  explicit ModuleError(const char* msg) {
    error_code = kError;
    size_t len = strlen(msg) + 1;
    char* result = new char[len];
    strncpy(result, msg, len);
    error_msg.Reset(result);
  }
};


// Helpers for nice error messages.
class FunctionError : public FunctionResult {
 public:
  explicit FunctionError(const char* msg) {
    error_code = kError;
    size_t len = strlen(msg) + 1;
    char* result = new char[len];
    strncpy(result, msg, len);
    error_msg.Reset(result);
  }
};


ModuleResult DecodeWasmModule(Isolate* isolate, Zone* zone,
                              const byte* module_start, const byte* module_end,
                              bool verify_functions) {
  size_t size = module_end - module_start;
  if (module_start > module_end) return ModuleError("start > end");
  if (size < kMinModuleSize) return ModuleError("size < minimum module size");
  if (size >= kMaxModuleSize) return ModuleError("size > maximum module size");
  WasmModule* module = new WasmModule();
  ModuleDecoder decoder(zone, module_start, module_end);
  return decoder.DecodeModule(module, verify_functions);
}


FunctionSig* DecodeFunctionSignatureForTesting(Zone* zone, const byte* start,
                                               const byte* end) {
  ModuleDecoder decoder(zone, start, end);
  return decoder.DecodeFunctionSignature(start);
}


FunctionResult DecodeWasmFunction(Isolate* isolate, Zone* zone,
                                  ModuleEnv* module_env,
                                  const byte* function_start,
                                  const byte* function_end) {
  size_t size = function_end - function_start;
  if (function_start > function_end) return FunctionError("start > end");
  if (size > kMaxFunctionSize)
    return FunctionError("size > maximum function size");
  WasmFunction* function = new WasmFunction();
  ModuleDecoder decoder(zone, function_start, function_end);
  return decoder.DecodeSingleFunction(module_env, function);
}


int32_t CompileAndRunWasmModule(Isolate* isolate, const byte* module_start,
                                const byte* module_end) {
  HandleScope scope(isolate);
  Zone zone;
  // Decode the module, but don't verify function bodies, since we'll
  // be compiling them anyway.
  ModuleResult result =
      DecodeWasmModule(isolate, &zone, module_start, module_end, false);
  if (result.failed()) {
    // Module verification failed. throw.
    std::ostringstream str;
    str << "WASM.compileRun() failed: " << result;
    isolate->Throw(
        *isolate->factory()->NewStringFromAsciiChecked(str.str().c_str()));
    return -1;
  }

  int32_t retval = CompileAndRunWasmModule(isolate, result.val);
  delete result.val;
  return retval;
}


int32_t CompileAndRunWasmModule(Isolate* isolate, WasmModule* module) {
  // Allocate temporary linear memory and globals.
  // TODO(titzer): 128mb is just a hack until the memory size is encoded.
  size_t mem_size = module->mem_size_log2 == 0 ? 128 * 1024 * 1024
                                               : 1 << module->mem_size_log2;
  size_t globals_size = AllocateGlobalsOffsets(module->globals);

  // TODO(titzer): use embedder API to allocate internal module?
  base::SmartArrayPointer<byte> mem_addr(new byte[mem_size]);
  base::SmartArrayPointer<byte> globals_addr(new byte[globals_size]);

  memset(mem_addr.get(), 0, mem_size);
  memset(globals_addr.get(), 0, globals_size);

  // Create module environment.
  ModuleEnv module_env;
  module_env.module = module;
  module_env.mem_start = reinterpret_cast<uintptr_t>(mem_addr.get());
  module_env.mem_end = reinterpret_cast<uintptr_t>(mem_addr.get()) + mem_size;
  module_env.globals_area = reinterpret_cast<uintptr_t>(globals_addr.get());
  std::vector<Handle<Code>> function_code(module->functions->size());
  module_env.function_code = &function_code;

  // Load data segments.
  // TODO(titzer): throw instead of crashing if segments don't fit in memory?
  LoadDataSegments(module, mem_addr.get(), mem_size);

  // Compile all functions.
  Handle<Code> main_code = Handle<Code>::null();  // record last code.
  int index = 0;
  for (const WasmFunction& func : *module->functions) {
    if (!func.external) {
      // Compile the function and install it in the code table.
      Handle<Code> code = CompileFunction(isolate, &module_env, func, index);
      function_code[index] = code;
      if (!code.is_null() && func.exported) main_code = code;
    }
    index++;
  }

  if (!main_code.is_null()) {
    // Run the main code.
    int32_t (*raw_func)() = reinterpret_cast<int (*)()>(main_code->entry());
    return raw_func();
  } else {
    // No main code was found.
    isolate->Throw(*isolate->factory()->NewStringFromStaticChars(
        "WASM.compileRun() failed: no valid main code produced."));
  }
  return -1;
}
}
}
}
