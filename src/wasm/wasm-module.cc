
#include "src/v8.h"

#include "src/wasm/wasm-module.h"

namespace v8 {
namespace internal {
namespace wasm {

// Internal constants for the layout of the module object.
static const int kWasmModuleInternalFieldCount = 4;
static const int kWasmModuleFunctionTable = 0;
static const int kWasmModuleCodeTable = 1;
static const int kWasmHeapArrayBuffer = 2;
static const int kWasmGlobalsArrayBuffer = 3;


// Instantiates a wasm module as a JSObject.
//  * allocates a backing store of {heap_size} bytes.
//  * installs a named property for that buffer if exported
//  * installs named properties on the object for exported functions
//  * compiles wasm code to machine code
MaybeHandle<JSObject> WasmModule::Instantiate(Isolate* isolate) {
  Factory* factory = isolate->factory();
  Handle<Map> map = factory->NewMap(
      JS_OBJECT_TYPE,
      JSObject::kHeaderSize + kWasmModuleInternalFieldCount * kPointerSize);

  // Allocate the module object.
  Handle<JSObject> module = factory->NewJSObjectFromMap(map, TENURED);
  Handle<FixedArray> code_table =
      factory->NewFixedArray(static_cast<int>(functions->size()), TENURED);

  // Allocate the raw memory for the heap.
  if (heap_size_log2 > kMaxHeapSize) {
    // Heap is bigger than maximum supported size.
    return isolate->Throw<JSObject>(
        factory->InternalizeUtf8String("Out of memory: wasm heap too large"));
  }
  uint32_t size = 1 << heap_size_log2;
  void* backing_store = isolate->array_buffer_allocator()->Allocate(size);
  if (!backing_store) {
    // Not enough space for backing store of heap
    return isolate->Throw<JSObject>(
        factory->InternalizeUtf8String("Out of memory: wasm heap"));
  }

#if DEBUG
  // Double check the API allocator actually zero-initialized the heap.
  for (uint32_t i = 0; i < size; i++) {
    DCHECK_EQ(0, static_cast<byte*>(backing_store)[i]);
  }
#endif

  // Load initialized data segments.
  for (const WasmDataSegment& segment : *data_segments) {
    if (!segment.init) continue;
    CHECK_LT(segment.dest_addr, size);
    CHECK_LT(segment.source_size, size);
    CHECK_LT(segment.dest_addr + segment.source_size, size);
    byte* addr = reinterpret_cast<byte*>(backing_store) + segment.dest_addr;
    memcpy(addr, module_start + segment.source_offset, segment.source_size);
  }

  // Create the JSArrayBuffer backed by the raw memory.
  isolate->heap()->RegisterNewArrayBuffer(backing_store, size);
  Handle<JSArrayBuffer> buffer = factory->NewJSArrayBuffer();
  buffer->set_backing_store(backing_store);
  buffer->set_is_external(false);
  buffer->set_is_neuterable(false);
  buffer->set_byte_length(Smi::FromInt(size));
  module->SetInternalField(kWasmHeapArrayBuffer, *buffer);

  // TODO: allocate storage for the globals.
  module->SetInternalField(kWasmGlobalsArrayBuffer, Smi::FromInt(0));

  if (heap_export) {
    // Export the heap as a named property.
    Handle<String> name = factory->InternalizeUtf8String("buffer");
    JSObject::AddProperty(module, name, buffer, READ_ONLY);
  }

  // Process functions in the module.
  int index = 0;
  for (const WasmFunction& func : *functions) {
    // Compile each function and initialize the code table.
    Handle<String> name =
        factory->InternalizeUtf8String(GetName(func.name_offset));
    if (func.external) {
      // External functions are read-write properties on this object.
      Handle<Object> undefined = isolate->factory()->undefined_value();
      JSObject::AddProperty(module, name, undefined, DONT_DELETE);
    } else {
      // Compile the function and install it in the code table.
      Handle<Code> code = Compile(index, func);
      code_table->set(index, *code);
    }
    if (func.exported) {
      // Export functions are installed as read-only properties on the module.
      Handle<JSFunction> function = factory->NewFunction(name);
      JSObject::AddProperty(module, name, function, READ_ONLY);
    }
    index++;
  }

  module->SetInternalField(kWasmModuleFunctionTable, Smi::FromInt(0));
  module->SetInternalField(kWasmModuleCodeTable, *code_table);
  return module;
}


Handle<Code> WasmModule::Compile(int index, const WasmFunction& function) {
  return Handle<Code>::null();  // TODO
}
}
}
}
