// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/wasm/wasm-wrapper.h"
#include "src/wasm/tf-builder.h"

#include "src/compiler/pipeline.h"
#include "src/compiler/machine-operator.h"
#include "src/compiler/change-lowering.h"
#include "src/compiler/js-generic-lowering.h"
#include "src/compiler/js-operator.h"
#include "src/compiler/js-graph.h"
#include "src/compiler/common-operator.h"
#include "src/compiler/graph.h"
#include "src/compiler/linkage.h"
#include "src/compiler/source-position.h"
#include "src/compiler/graph-visualizer.h"
#include "src/compiler/typer.h"

namespace v8 {
namespace internal {
namespace wasm {

Handle<JSFunction> CompileJSToWasmWrapper(Isolate* isolate, ModuleEnv* module,
                                          Handle<String> name,
                                          Handle<Code> wasm_code,
                                          uint32_t index) {
  WasmFunction* func = &module->module->functions->at(index);

  //----------------------------------------------------------------------------
  // Create the JSFunction object.
  //----------------------------------------------------------------------------
  Handle<SharedFunctionInfo> shared =
      isolate->factory()->NewSharedFunctionInfo(name, wasm_code);
  int params = static_cast<int>(func->sig->parameter_count());
  shared->set_length(params);
  shared->set_internal_formal_parameter_count(1 + params);
  Handle<JSFunction> function = isolate->factory()->NewFunction(name);
  function->set_shared(*shared);

  //----------------------------------------------------------------------------
  // Create the TFGraph
  //----------------------------------------------------------------------------
  Zone zone;
  compiler::Graph graph(&zone);
  compiler::CommonOperatorBuilder common(&zone);
  compiler::JSOperatorBuilder javascript(&zone);
  compiler::MachineOperatorBuilder machine(&zone);
  compiler::JSGraph jsgraph(isolate, &graph, &common, &javascript, &machine);

  TFNode* control = nullptr;
  TFNode* effect = nullptr;

  TFBuilder builder(&zone, &jsgraph);
  builder.control = &control;
  builder.effect = &effect;
  builder.module = module;
  builder.BuildJSToWasmWrapper(wasm_code, func->sig);

  //----------------------------------------------------------------------------
  // Run the compilation pipeline.
  //----------------------------------------------------------------------------
  {
    // Changes lowering requires types.
    compiler::Typer typer(isolate, &graph);
    compiler::NodeVector roots(&zone);
    jsgraph.GetCachedNodes(&roots);
    typer.Run(roots);

    // Run generic and change lowering.
    compiler::JSGenericLowering generic(true, &jsgraph);
    compiler::ChangeLowering changes(&jsgraph);
    compiler::GraphReducer graph_reducer(&zone, &graph, jsgraph.Dead());
    graph_reducer.AddReducer(&changes);
    graph_reducer.AddReducer(&generic);
    graph_reducer.ReduceGraph();

    if (FLAG_trace_turbo_graph) {  // Simple textual RPO.
      OFStream os(stdout);
      os << "-- Graph after change lowering -- " << std::endl;
      os << compiler::AsRPO(graph);
    }

    // Schedule and compile to machine code.
    int params = static_cast<int>(
        module->GetFunctionSignature(index)->parameter_count());
    compiler::CallDescriptor* incoming = compiler::Linkage::GetJSCallDescriptor(
        &zone, false, params + 1, compiler::CallDescriptor::kNoFlags);
    CompilationInfo info("js-to-wasm", isolate, &zone);
    info.set_output_code_kind(Code::OPTIMIZED_FUNCTION);
    Handle<Code> code = compiler::Pipeline::GenerateCodeForTesting(
        &info, incoming, &graph, nullptr);

#ifdef ENABLE_DISASSEMBLER
    // Disassemble the wrapper code for debugging.
    if (!code.is_null() && FLAG_print_opt_code) {
      static const int kBufferSize = 128;
      char buffer[kBufferSize];
      const char* name = "";
      if (func->name_offset > 0) {
        const byte* ptr = module->module->module_start + func->name_offset;
        name = reinterpret_cast<const char*>(ptr);
      }
      snprintf(buffer, kBufferSize, "JS->WASM function wrapper #%d:%s", index,
               name);
      OFStream os(stdout);
      code->Disassemble(buffer, os);
    }
#endif
    // Set the JSFunction's machine code.
    function->set_code(*code);
  }
  return function;
}


Handle<Code> CompileWasmToJSWrapper(Isolate* isolate, ModuleEnv* module,
                                    Handle<JSFunction> function,
                                    uint32_t index) {
  WasmFunction* func = &module->module->functions->at(index);

  //----------------------------------------------------------------------------
  // Create the TFGraph
  //----------------------------------------------------------------------------
  Zone zone;
  compiler::Graph graph(&zone);
  compiler::CommonOperatorBuilder common(&zone);
  compiler::JSOperatorBuilder javascript(&zone);
  compiler::MachineOperatorBuilder machine(&zone);
  compiler::JSGraph jsgraph(isolate, &graph, &common, &javascript, &machine);

  TFNode* control = nullptr;
  TFNode* effect = nullptr;

  TFBuilder builder(&zone, &jsgraph);
  builder.control = &control;
  builder.effect = &effect;
  builder.module = module;
  builder.BuildWasmToJSWrapper(function, func->sig);

  Handle<Code> code = Handle<Code>::null();
  {
    // Changes lowering requires types.
    compiler::Typer typer(isolate, &graph);
    compiler::NodeVector roots(&zone);
    jsgraph.GetCachedNodes(&roots);
    typer.Run(roots);

    // Run generic and change lowering.
    compiler::JSGenericLowering generic(true, &jsgraph);
    compiler::ChangeLowering changes(&jsgraph);
    compiler::GraphReducer graph_reducer(&zone, &graph, jsgraph.Dead());
    graph_reducer.AddReducer(&changes);
    graph_reducer.AddReducer(&generic);
    graph_reducer.ReduceGraph();

    if (FLAG_trace_turbo_graph) {  // Simple textual RPO.
      OFStream os(stdout);
      os << "-- Graph after change lowering -- " << std::endl;
      os << compiler::AsRPO(graph);
    }

    // Schedule and compile to machine code.
    compiler::CallDescriptor* incoming =
        module->GetWasmCallDescriptor(&zone, func->sig);
    CompilationInfo info("wasm-to-js", isolate, &zone);
    code = compiler::Pipeline::GenerateCodeForTesting(&info, incoming, &graph,
                                                      nullptr);

#ifdef ENABLE_DISASSEMBLER
    // Disassemble the wrapper code for debugging.
    if (!code.is_null() && FLAG_print_opt_code) {
      static const int kBufferSize = 128;
      char buffer[kBufferSize];
      const char* name = "";
      if (func->name_offset > 0) {
        const byte* ptr = module->module->module_start + func->name_offset;
        name = reinterpret_cast<const char*>(ptr);
      }
      snprintf(buffer, kBufferSize, "WASM->JS function wrapper #%d:%s", index,
               name);
      OFStream os(stdout);
      code->Disassemble(buffer, os);
    }
#endif
  }
  return code;
}
}
}
}
