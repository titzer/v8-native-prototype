// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/common-operator.h"
#include "src/compiler/js-graph.h"
#include "src/compiler/machine-operator.h"
#include "src/compiler/simplified-operator.h"

#include "src/code-stubs.h"

#include "src/compiler/linkage.h"

#include "src/wasm/tf-builder.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"


// TODO(titzer): pull WASM_64 up to a common header.
#if !V8_TARGET_ARCH_32_BIT || V8_TARGET_ARCH_X64
#define WASM_64 1
#else
#define WASM_64 0
#endif

namespace v8 {
namespace internal {
namespace wasm {

static compiler::MachineType MachineTypeFor(LocalType type) {
  switch (type) {
    case kAstInt32:
      return compiler::kMachInt32;
    case kAstInt64:
      return compiler::kMachInt64;
    case kAstFloat64:
      return compiler::kMachFloat64;
    case kAstFloat32:
      return compiler::kMachFloat32;
    default:
      UNREACHABLE();
      return compiler::kMachAnyTagged;
  }
}


static compiler::MachineType MachineTypeFor(MemType type) {
  switch (type) {
    case kMemInt8:
      return compiler::kMachInt8;
    case kMemInt16:
      return compiler::kMachInt16;
    case kMemInt32:
      return compiler::kMachInt32;
    case kMemInt64:
      return compiler::kMachInt64;
    case kMemUint8:
      return compiler::kMachUint8;
    case kMemUint16:
      return compiler::kMachUint16;
    case kMemUint32:
      return compiler::kMachUint32;
    case kMemUint64:
      return compiler::kMachUint64;
    case kMemFloat64:
      return compiler::kMachFloat64;
    case kMemFloat32:
      return compiler::kMachFloat32;
    default:
      UNREACHABLE();
      return compiler::kMachAnyTagged;
  }
}


static void MergeControlToEnd(TFGraph* graph, TFNode* node) {
  compiler::Graph* g = graph->graph();
  if (g->end()) {
    compiler::NodeProperties::MergeControlToEnd(g, graph->common(), node);
  } else {
    g->SetEnd(g->NewNode(graph->common()->End(1), node));
  }
}

TFBuilder::TFBuilder(Zone* z, TFGraph* g)
    : zone(z),
      graph(g),
      module(nullptr),
      mem_buffer(nullptr),
      mem_size(nullptr),
      control(nullptr),
      effect(nullptr),
      cur_buffer(def_buffer),
      cur_bufsize(kDefaultBufferSize) {}

TFNode* TFBuilder::Error() {
  if (!graph) return nullptr;
  return graph->Dead();
}


TFNode* TFBuilder::Start(unsigned params) {
  if (!graph) return nullptr;
  compiler::Graph* g = graph->graph();
  TFNode* start = g->NewNode(graph->common()->Start(params));
  g->SetStart(start);
  return start;
}


TFNode* TFBuilder::Param(unsigned index, LocalType type) {
  if (!graph) return nullptr;
  compiler::Graph* g = graph->graph();
  // TODO(titzer): use LocalType for parameters
  return g->NewNode(graph->common()->Parameter(index), g->start());
}


TFNode* TFBuilder::Loop(TFNode* entry) {
  return graph ? graph->graph()->NewNode(graph->common()->Loop(1), entry)
               : nullptr;
}


TFNode* TFBuilder::Terminate(TFNode* effect, TFNode* control) {
  if (!graph) return nullptr;
  TFNode* terminate =
      graph->graph()->NewNode(graph->common()->Terminate(), effect, control);
  MergeControlToEnd(graph, terminate);
  return terminate;
}


unsigned TFBuilder::InputCount(TFNode* node) {
  return static_cast<unsigned>(node->InputCount());
}


bool TFBuilder::IsPhiWithMerge(TFNode* phi, TFNode* merge) {
  return phi && compiler::IrOpcode::IsPhiOpcode(phi->opcode()) &&
         compiler::NodeProperties::GetControlInput(phi) == merge;
}


void TFBuilder::AppendToMerge(TFNode* merge, TFNode* from) {
  if (graph) {
    DCHECK(compiler::IrOpcode::IsMergeOpcode(merge->opcode()));
    merge->AppendInput(graph->zone(), from);
    merge->set_op(
        graph->common()->ResizeMergeOrPhi(merge->op(), merge->InputCount()));
  }
}


void TFBuilder::AppendToPhi(TFNode* merge, TFNode* phi, TFNode* from) {
  if (graph) {
    DCHECK(compiler::IrOpcode::IsPhiOpcode(phi->opcode()));
    DCHECK(compiler::IrOpcode::IsMergeOpcode(merge->opcode()));
    phi->set_op(
        graph->common()->ResizeMergeOrPhi(phi->op(), phi->InputCount()));
    phi->InsertInput(graph->zone(), phi->InputCount() - 1, from);
  }
}


TFNode* TFBuilder::Merge(unsigned count, TFNode** controls) {
  if (!graph) return nullptr;
  return graph->graph()->NewNode(graph->common()->Merge(count), count,
                                 controls);
}


TFNode* TFBuilder::Phi(LocalType type, unsigned count, TFNode** vals,
                       TFNode* control) {
  if (!graph) return nullptr;
  TFNode** buf = Buffer(count + 1);
  if (buf != vals) memcpy(buf, vals, sizeof(TFNode*) * count);
  buf[count] = control;
  compiler::MachineType machine_type = MachineTypeFor(type);
  return graph->graph()->NewNode(graph->common()->Phi(machine_type, count),
                                 count + 1, buf);
}


TFNode* TFBuilder::EffectPhi(unsigned count, TFNode** effects,
                             TFNode* control) {
  if (!graph) return nullptr;
  TFNode** buf = Buffer(count + 1);
  if (buf != effects) memcpy(buf, effects, sizeof(TFNode*) * count);
  buf[count] = control;
  return graph->graph()->NewNode(graph->common()->EffectPhi(count), count + 1,
                                 buf);
}


TFNode* TFBuilder::Int32Constant(int32_t value) {
  return graph ? graph->Int32Constant(value) : nullptr;
}


TFNode* TFBuilder::Int64Constant(int64_t value) {
  return graph ? graph->Int64Constant(value) : nullptr;
}


static const compiler::Operator* UnsupportedOpcode(WasmOpcode opcode) {
  if (WasmOpcodes::IsSupported(opcode)) {
    V8_Fatal(__FILE__, __LINE__,
             "Unsupported opcode #%d:%s reported as supported", opcode,
             WasmOpcodes::OpcodeName(opcode));
  }
  V8_Fatal(__FILE__, __LINE__, "Unsupported opcode #%d:%s", opcode,
           WasmOpcodes::OpcodeName(opcode));
  return nullptr;
}


TFNode* TFBuilder::Binop(WasmOpcode opcode, TFNode* left, TFNode* right) {
  // TODO(titzer): insert manual divide-by-zero checks.
  if (!graph) return nullptr;
  const compiler::Operator* op;
  compiler::MachineOperatorBuilder* m = graph->machine();
  switch (opcode) {
    case kExprInt32Add:
      op = m->Int32Add();
      break;
    case kExprInt32Sub:
      op = m->Int32Sub();
      break;
    case kExprInt32Mul:
      op = m->Int32Mul();
      break;
    case kExprInt32SDiv:
      op = m->Int32Div();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt32UDiv:
      op = m->Uint32Div();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt32SRem:
      op = m->Int32Mod();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt32URem:
      op = m->Uint32Mod();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt32And:
      op = m->Word32And();
      break;
    case kExprInt32Ior:
      op = m->Word32Or();
      break;
    case kExprInt32Xor:
      op = m->Word32Xor();
      break;
    case kExprInt32Shl:
      op = m->Word32Shl();
      break;
    case kExprInt32Shr:
      op = m->Word32Shr();
      break;
    case kExprInt32Sar:
      op = m->Word32Sar();
      break;
    case kExprInt32Eq:
      op = m->Word32Equal();
      break;
    case kExprInt32Ne:
      return Invert(Binop(kExprInt32Eq, left, right));
    case kExprInt32Slt:
      op = m->Int32LessThan();
      break;
    case kExprInt32Sle:
      op = m->Int32LessThanOrEqual();
      break;
    case kExprInt32Ult:
      op = m->Uint32LessThan();
      break;
    case kExprInt32Ule:
      op = m->Uint32LessThanOrEqual();
      break;
    case kExprInt32Sgt:
      op = m->Int32LessThan();
      std::swap(left, right);
      break;
    case kExprInt32Sge:
      op = m->Int32LessThanOrEqual();
      std::swap(left, right);
      break;
    case kExprInt32Ugt:
      op = m->Uint32LessThan();
      std::swap(left, right);
      break;
    case kExprInt32Uge:
      op = m->Uint32LessThanOrEqual();
      std::swap(left, right);
      break;
#if WASM_64
    // Opcodes only supported on 64-bit platforms.
    // TODO(titzer): query the machine operator builder here instead of #ifdef.
    case kExprInt64Add:
      op = m->Int64Add();
      break;
    case kExprInt64Sub:
      op = m->Int64Sub();
      break;
    case kExprInt64Mul:
      op = m->Int64Mul();
      break;
    case kExprInt64SDiv:
      op = m->Int64Div();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt64UDiv:
      op = m->Uint64Div();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt64SRem:
      op = m->Int64Mod();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt64URem:
      op = m->Uint64Mod();
      return graph->graph()->NewNode(op, left, right, *control);
    case kExprInt64And:
      op = m->Word64And();
      break;
    case kExprInt64Ior:
      op = m->Word64Or();
      break;
    case kExprInt64Xor:
      op = m->Word64Xor();
      break;
    case kExprInt64Shl:
      op = m->Word64Shl();
      break;
    case kExprInt64Shr:
      op = m->Word64Shr();
      break;
    case kExprInt64Sar:
      op = m->Word64Sar();
      break;
    case kExprInt64Eq:
      op = m->Word64Equal();
      break;
    case kExprInt64Ne:
      return Invert(Binop(kExprInt64Eq, left, right));
    case kExprInt64Slt:
      op = m->Int64LessThan();
      break;
    case kExprInt64Sle:
      op = m->Int64LessThanOrEqual();
      break;
    case kExprInt64Ult:
      op = m->Uint64LessThan();
      break;
    case kExprInt64Ule:
      op = m->Uint64LessThanOrEqual();
      break;
    case kExprInt64Sgt:
      op = m->Int64LessThan();
      std::swap(left, right);
      break;
    case kExprInt64Sge:
      op = m->Int64LessThanOrEqual();
      std::swap(left, right);
      break;
    case kExprInt64Ugt:
      op = m->Uint64LessThan();
      std::swap(left, right);
      break;
    case kExprInt64Uge:
      op = m->Uint64LessThanOrEqual();
      std::swap(left, right);
      break;
#endif

    case kExprFloat32Add:
      op = m->Float32Add();
      break;
    case kExprFloat32Sub:
      op = m->Float32Sub();
      break;
    case kExprFloat32Mul:
      op = m->Float32Mul();
      break;
    case kExprFloat32Div:
      op = m->Float32Div();
      break;
    case kExprFloat32Eq:
      op = m->Float32Equal();
      break;
    case kExprFloat32Ne:
      return Invert(Binop(kExprFloat32Eq, left, right));
    case kExprFloat32Lt:
      op = m->Float32LessThan();
      break;
    case kExprFloat32Ge:
      op = m->Float32LessThanOrEqual();
      std::swap(left, right);
      break;
    case kExprFloat32Gt:
      op = m->Float32LessThan();
      std::swap(left, right);
      break;
    case kExprFloat32Le:
      op = m->Float32LessThanOrEqual();
      break;
    case kExprFloat64Add:
      op = m->Float64Add();
      break;
    case kExprFloat64Sub:
      op = m->Float64Sub();
      break;
    case kExprFloat64Mul:
      op = m->Float64Mul();
      break;
    case kExprFloat64Div:
      op = m->Float64Div();
      break;
    case kExprFloat64Eq:
      op = m->Float64Equal();
      break;
    case kExprFloat64Ne:
      return Invert(Binop(kExprFloat64Eq, left, right));
    case kExprFloat64Lt:
      op = m->Float64LessThan();
      break;
    case kExprFloat64Le:
      op = m->Float64LessThanOrEqual();
      break;
    case kExprFloat64Gt:
      op = m->Float64LessThan();
      std::swap(left, right);
      break;
    case kExprFloat64Ge:
      op = m->Float64LessThanOrEqual();
      std::swap(left, right);
      break;
    default:
      op = UnsupportedOpcode(opcode);
  }
  return graph->graph()->NewNode(op, left, right);
}


TFNode* TFBuilder::Unop(WasmOpcode opcode, TFNode* input) {
  if (!graph) return nullptr;
  const compiler::Operator* op;
  compiler::MachineOperatorBuilder* m = graph->machine();
  switch (opcode) {
    case kExprBoolNot:
      op = m->Word32Equal();
      return graph->graph()->NewNode(op, input, graph->ZeroConstant());
    case kExprFloat32Abs:
      op = m->Float32Abs();
      break;
    case kExprFloat32Neg:
      op = m->Float32Sub();
      return graph->graph()->NewNode(op, graph->Float32Constant(0), input);
    case kExprFloat32Sqrt:
      op = m->Float32Sqrt();
      break;
    case kExprFloat64Abs:
      op = m->Float64Abs();
      break;
    case kExprFloat64Neg:
      op = m->Float64Sub();
      return graph->graph()->NewNode(op, graph->Float64Constant(0), input);
    case kExprFloat64Sqrt:
      op = m->Float64Sqrt();
      break;
    case kExprInt32SConvertFloat64:
      op = m->ChangeFloat64ToInt32();
      break;
    case kExprInt32UConvertFloat64:
      op = m->ChangeFloat64ToUint32();
      break;
    case kExprFloat32ConvertFloat64:
      op = m->TruncateFloat64ToFloat32();
      break;
    case kExprFloat64SConvertInt32:
      op = m->ChangeInt32ToFloat64();
      break;
    case kExprFloat64UConvertInt32:
      op = m->ChangeUint32ToFloat64();
      break;
    case kExprFloat32SConvertInt32:
      op = m->ChangeInt32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->TruncateFloat64ToFloat32();
      break;
    case kExprFloat32UConvertInt32:
      op = m->ChangeUint32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->TruncateFloat64ToFloat32();
      break;
    case kExprInt32SConvertFloat32:
      op = m->ChangeFloat32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->ChangeFloat64ToInt32();
      break;
    case kExprInt32UConvertFloat32:
      op = m->ChangeFloat32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->ChangeFloat64ToUint32();
      break;
    case kExprFloat64ConvertFloat32:
      op = m->ChangeFloat32ToFloat64();
      break;
#if WASM_64
    // Opcodes only supported on 64-bit platforms.
    // TODO(titzer): query the machine operator builder here instead of #ifdef.
    case kExprInt32ConvertInt64:
      op = m->TruncateInt64ToInt32();
      break;
    case kExprInt64SConvertInt32:
      op = m->ChangeInt32ToInt64();
      break;
    case kExprInt64UConvertInt32:
      op = m->ChangeUint32ToUint64();
      break;
#endif
    default:
      op = UnsupportedOpcode(opcode);
  }
  return graph->graph()->NewNode(op, input);
}


TFNode* TFBuilder::Float32Constant(float value) {
  return graph ? graph->Float32Constant(value) : nullptr;
}


TFNode* TFBuilder::Float64Constant(double value) {
  return graph ? graph->Float64Constant(value) : nullptr;
}


TFNode* TFBuilder::Constant(Handle<Object> value) {
  return graph ? graph->Constant(value) : nullptr;
}


void TFBuilder::Branch(TFNode* cond, TFNode** true_node, TFNode** false_node) {
  if (!graph) return;
  DCHECK_NOT_NULL(*control);
  TFNode* branch =
      graph->graph()->NewNode(graph->common()->Branch(), cond, *control);
  *true_node = graph->graph()->NewNode(graph->common()->IfTrue(), branch);
  *false_node = graph->graph()->NewNode(graph->common()->IfFalse(), branch);
}


void TFBuilder::Return(unsigned count, TFNode** vals) {
  if (!graph) return;
  DCHECK_NOT_NULL(*control);
  DCHECK_NOT_NULL(*effect);

  if (count == 0) {
    // Handle a return of void.
    vals[0] = graph->ZeroConstant();
    count = 1;
  }

  compiler::Graph* g = graph->graph();
  TFNode** buf = Buffer(count + 2);
  if (buf != vals) memcpy(buf, vals, sizeof(TFNode*) * count);
  buf[count] = *effect;
  buf[count + 1] = *control;
  TFNode* ret = g->NewNode(graph->common()->Return(), count + 2, vals);

  MergeControlToEnd(graph, ret);
}


void TFBuilder::ReturnVoid() {
  TFNode** vals = Buffer(0);
  Return(0, vals);
}


TFNode* TFBuilder::CallDirect(uint32_t index, TFNode** args) {
  DCHECK_NULL(args[0]);
  if (!graph) return nullptr;

  FunctionSig* sig = module->GetFunctionSignature(index);
  const size_t params = sig->parameter_count();
  const size_t extra = 2;  // effect and control inputs.
  const size_t count = 1 + params + extra;

  if (args != cur_buffer || cur_bufsize < count) {
    // Reallocate the buffer to make space for extra inputs.
    TFNode** nargs = Buffer(count);
    memcpy(nargs + 1, args + 1, params * sizeof(TFNode**));
    args = nargs;
  }

  // Add code object as constant.
  // TODO(titzer): handle JS, external calls with framestate.
  args[0] = Constant(module->GetFunctionCode(index));
  // Add effect and control inputs.
  args[params + 1] = *effect;
  args[params + 2] = *control;

  const compiler::Operator* op =
      graph->common()->Call(module->GetCallDescriptor(graph->zone(), index));
  // TODO(titzer): handle JS, external calls with framestate.
  TFNode* call = graph->graph()->NewNode(op, static_cast<int>(count), args);

  *effect = call;
  return call;
}


TFNode* TFBuilder::CallIndirect(uint32_t index, TFNode** args) {
  DCHECK_NULL(args[0]);
  UNIMPLEMENTED();
  return nullptr;
}


TFNode* TFBuilder::ToJS(TFNode* node, TFNode* context, LocalType type) {
  if (!graph) return nullptr;
  compiler::Graph* g = graph->graph();
  compiler::SimplifiedOperatorBuilder simplified(graph->zone());
  switch (type) {
    case kAstInt32:
      return g->NewNode(simplified.ChangeInt32ToTagged(), node);
    case kAstInt64:
      UNIMPLEMENTED();
      return node;
    case kAstFloat32:
      node = g->NewNode(graph->machine()->ChangeFloat32ToFloat64(), node);
      return g->NewNode(simplified.ChangeFloat64ToTagged(), node);
    case kAstFloat64:
      return g->NewNode(simplified.ChangeFloat64ToTagged(), node);
    case kAstStmt:
      return graph->UndefinedConstant();
  }
}


TFNode* TFBuilder::FromJS(TFNode* node, TFNode* context, LocalType type) {
  if (!graph) return nullptr;
  compiler::Graph* g = graph->graph();
  // Do a JavaScript ToNumber.
  TFNode* num = g->NewNode(graph->javascript()->ToNumber(), node, context,
                           graph->EmptyFrameState(), *effect, *control);
  *control = num;
  *effect = num;

  // Change representation.
  compiler::SimplifiedOperatorBuilder simplified(graph->zone());
  num = g->NewNode(simplified.ChangeTaggedToFloat64(), num);

  switch (type) {
    case kAstInt32: {
      num = g->NewNode(graph->machine()->TruncateFloat64ToInt32(
                           compiler::TruncationMode::kJavaScript),
                       num);
      break;
    }
    case kAstInt64:
      UNIMPLEMENTED();
      break;
    case kAstFloat32:
      num = g->NewNode(graph->machine()->TruncateFloat64ToFloat32(), num);
      break;
    case kAstFloat64:
      break;
    case kAstStmt:
      num = graph->Int32Constant(0);
      break;
  }
  return num;
}


TFNode* TFBuilder::Invert(TFNode* node) {
  if (!graph) return nullptr;
  return Unop(kExprBoolNot, node);
}


void TFBuilder::BuildJSToWasmWrapper(Handle<Code> wasm_code, FunctionSig* sig) {
  CHECK_NOT_NULL(graph);

  int params = static_cast<int>(sig->parameter_count());
  compiler::Graph* g = graph->graph();
  int count = params + 3;
  TFNode** args = Buffer(count);

  // Build the start and the JS parameter nodes.
  TFNode* start = Start(params + 3);
  *control = start;
  *effect = start;
  // JS context is the last parameter.
  TFNode* context =
      g->NewNode(graph->common()->Parameter(params + 1, "context"), start);

  int pos = 0;
  args[pos++] = Constant(wasm_code);

  // Convert JS parameters to WASM numbers.
  for (int i = 0; i < params; i++) {
    TFNode* param = g->NewNode(graph->common()->Parameter(i), start);
    args[pos++] = FromJS(param, context, sig->GetParam(i));
  }

  args[pos++] = *effect;
  args[pos++] = *control;

  // Call the WASM code.
  compiler::CallDescriptor* desc =
      module->GetWasmCallDescriptor(graph->zone(), sig);
  TFNode* call = g->NewNode(graph->common()->Call(desc), count, args);
  TFNode* jsval = ToJS(call, context,
                       sig->return_count() == 0 ? kAstStmt : sig->GetReturn());
  TFNode* ret = g->NewNode(graph->common()->Return(), jsval, call, start);

  MergeControlToEnd(graph, ret);
}


void TFBuilder::BuildWasmToJSWrapper(Handle<JSFunction> function,
                                     FunctionSig* sig) {
  CHECK_NOT_NULL(graph);
  int js_count = function->shared()->internal_formal_parameter_count();
  int wasm_count = static_cast<int>(sig->parameter_count());

  // Build the start and the parameter nodes.
  Isolate* isolate = graph->isolate();
  compiler::Graph* g = graph->graph();
  compiler::CallDescriptor* desc;
  TFNode* start = Start(wasm_count + 3);
  *effect = start;
  *control = start;
  // JS context is the last parameter.
  TFNode* context = Constant(Handle<Context>(function->context(), isolate));
  TFNode** args = Buffer(wasm_count + 6);

  int pos = 0;
  if (js_count == wasm_count) {
    // exact arity match, just call the function directly.
    desc = compiler::Linkage::GetJSCallDescriptor(
        g->zone(), false, 1 + wasm_count, compiler::CallDescriptor::kNoFlags);
  } else {
    // call through the CallFunctionStub to adapt arguments.
    CallFunctionFlags flags = NO_CALL_FUNCTION_FLAGS;
    CallFunctionStub stub(isolate, wasm_count, flags);
    CallInterfaceDescriptor d = stub.GetCallInterfaceDescriptor();

    args[pos++] = graph->HeapConstant(stub.GetCode());  // CallFunctionStub

    desc = compiler::Linkage::GetStubCallDescriptor(
        isolate, g->zone(), d, wasm_count + 1,
        compiler::CallDescriptor::kNoFlags);
  }

  args[pos++] = graph->Constant(function);   // JS function.
  args[pos++] = graph->UndefinedConstant();  // JS receiver.

  // Convert WASM numbers to JS values.
  for (int i = 0; i < wasm_count; i++) {
    TFNode* param = g->NewNode(graph->common()->Parameter(i), start);
    args[pos++] = ToJS(param, context, sig->GetParam(i));
  }


  args[pos++] = context;
  args[pos++] = *effect;
  args[pos++] = *control;

  TFNode* call = g->NewNode(graph->common()->Call(desc), pos, args);

  // Convert the return value back.
  TFNode* val = FromJS(call, context,
                       sig->return_count() == 0 ? kAstStmt : sig->GetReturn());
  TFNode* ret = g->NewNode(graph->common()->Return(), val, call, start);

  MergeControlToEnd(graph, ret);
}


TFNode* TFBuilder::MemBuffer() {
  if (!mem_buffer) mem_buffer = graph->IntPtrConstant(module->mem_start);
  return mem_buffer;
}


TFNode* TFBuilder::MemSize() {
  if (!mem_size)
    mem_size = graph->IntPtrConstant(module->mem_end - module->mem_start);
  return mem_size;
}


TFNode* TFBuilder::LoadGlobal(uint32_t index) {
  if (!graph) return nullptr;
  MemType mem_type = module->GetGlobalType(index);
  TFNode* addr = graph->IntPtrConstant(
      module->globals_area + module->module->globals->at(index).offset);
  const compiler::Operator* op =
      graph->machine()->Load(MachineTypeFor(mem_type));
  TFNode* node = graph->graph()->NewNode(op, addr, graph->ZeroConstant(),
                                         *effect, *control);
  *effect = node;
  return node;
}


TFNode* TFBuilder::StoreGlobal(uint32_t index, TFNode* val) {
  if (!graph) return nullptr;
  MemType mem_type = module->GetGlobalType(index);
  TFNode* addr = graph->IntPtrConstant(
      module->globals_area + module->module->globals->at(index).offset);
  const compiler::Operator* op =
      graph->machine()->Store(compiler::StoreRepresentation(
          MachineTypeFor(mem_type), compiler::kNoWriteBarrier));
  TFNode* node = graph->graph()->NewNode(op, addr, graph->ZeroConstant(), val,
                                         *effect, *control);
  *effect = node;
  return node;
}


TFNode* TFBuilder::LoadMem(MemType type, TFNode* index) {
  if (!graph) return nullptr;
  const compiler::Operator* op =
      graph->machine()->CheckedLoad(MachineTypeFor(type));
  TFNode* mem_buffer = MemBuffer();
  TFNode* mem_size = MemSize();
  TFNode* node = graph->graph()->NewNode(op, mem_buffer, index, mem_size,
                                         *effect, *control);
  *effect = node;
  return node;
}


TFNode* TFBuilder::StoreMem(MemType type, TFNode* index, TFNode* val) {
  if (!graph) return nullptr;
  const compiler::Operator* op =
      graph->machine()->CheckedStore(MachineTypeFor(type));
  TFNode* mem_buffer = MemBuffer();
  TFNode* mem_size = MemSize();
  TFNode* node = graph->graph()->NewNode(op, mem_buffer, index, mem_size, val,
                                         *effect, *control);
  *effect = node;
  return node;
}


void TFBuilder::PrintDebugName(TFNode* node) {
  PrintF("#%d:%s", node->id(), node->op()->mnemonic());
}
}
}
}
