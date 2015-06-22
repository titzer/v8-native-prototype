// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/compiler/js-graph.h"
#include "src/compiler/common-operator.h"
#include "src/compiler/machine-operator.h"

#include "src/wasm/wasm-opcodes.h"

#include "src/wasm/tf-builder.h"

namespace v8 {
namespace internal {
namespace wasm {

static compiler::MachineType MachineTypeFor(LocalType type) {
  switch (type) {
    case kAstInt32:
      return compiler::kMachInt32;
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
    case kMemUint8:
      return compiler::kMachUint8;
    case kMemUint16:
      return compiler::kMachUint16;
    case kMemUint32:
      return compiler::kMachUint32;
    case kMemFloat64:
      return compiler::kMachFloat64;
    case kMemFloat32:
      return compiler::kMachFloat32;
    default:
      UNREACHABLE();
      return compiler::kMachAnyTagged;
  }
}


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
  // TODO: use LocalType for parameters
  return g->NewNode(graph->common()->Parameter(index), g->start());
}


TFNode* TFBuilder::Loop(TFNode* entry) {
  return graph ? graph->graph()->NewNode(graph->common()->Loop(1), entry)
               : nullptr;
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


TFNode* TFBuilder::Int32Constant(int value) {
  return graph ? graph->Int32Constant(value) : nullptr;
}


TFNode* TFBuilder::Binop(WasmOpcode opcode, TFNode* left, TFNode* right) {
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
    case kExprFloat64Eq:
      op = m->Float64Equal();
      break;
    case kExprFloat64Lt:
      op = m->Float64LessThan();
      break;
    case kExprFloat64Le:
      op = m->Float64LessThanOrEqual();
      break;
    case kExprFloat32Eq:
      op = m->Float32Equal();
      break;
    case kExprFloat32Lt:
      op = m->Float32LessThan();
      break;
    case kExprFloat32Le:
      op = m->Float32LessThanOrEqual();
      break;
    default:
      UNREACHABLE();
      op = nullptr;
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
    case kExprInt32FromFloat64:
      op = m->ChangeFloat64ToInt32();
      break;
    case kExprUint32FromFloat64:
      op = m->ChangeFloat64ToUint32();
      break;
    case kExprFloat32FromFloat64:
      op = m->TruncateFloat64ToFloat32();
      break;
    case kExprFloat64FromSInt32:
      op = m->ChangeInt32ToFloat64();
      break;
    case kExprFloat64FromUInt32:
      op = m->ChangeUint32ToFloat64();
      break;
    case kExprFloat32FromSInt32:
      op = m->ChangeInt32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->TruncateFloat64ToFloat32();
      break;
    case kExprFloat32FromUInt32:
      op = m->ChangeUint32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->TruncateFloat64ToFloat32();
      break;
    case kExprInt32FromFloat32:
      op = m->ChangeFloat32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->ChangeFloat64ToInt32();
      break;
    case kExprUint32FromFloat32:
      op = m->ChangeFloat32ToFloat64();  // TODO(titzer): two conversions
      input = graph->graph()->NewNode(op, input);
      op = m->ChangeFloat64ToUint32();
      break;
    case kExprFloat64FromFloat32:
      op = m->ChangeFloat32ToFloat64();
      break;
    default:
      UNREACHABLE();
      op = nullptr;
  }
  return graph->graph()->NewNode(op, input);
}


TFNode* TFBuilder::Float32Constant(float value) {
  return graph ? graph->Float32Constant(value) : nullptr;
}


TFNode* TFBuilder::Float64Constant(double value) {
  return graph ? graph->Float64Constant(value) : nullptr;
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

  if (g->end()) {
    compiler::NodeProperties::MergeControlToEnd(g, graph->common(), ret);
  } else {
    g->SetEnd(g->NewNode(graph->common()->End(1), ret));
  }
}


TFNode* TFBuilder::HeapBuffer() {
  if (!heap_buffer) heap_buffer = graph->IntPtrConstant(heap_start);
  return heap_buffer;
}


TFNode* TFBuilder::HeapSize() {
  if (!heap_size) heap_size = graph->IntPtrConstant(heap_end - heap_start);
  return heap_size;
}


TFNode* TFBuilder::GetHeap(MemType type, TFNode* index) {
  if (!graph) return nullptr;
  const compiler::Operator* op =
      graph->machine()->CheckedLoad(MachineTypeFor(type));
  TFNode* heap_buffer = HeapBuffer();
  TFNode* heap_size = HeapSize();
  TFNode* node = graph->graph()->NewNode(op, heap_buffer, index, heap_size,
                                         *effect, *control);
  *effect = node;
  return node;
}


TFNode* TFBuilder::SetHeap(MemType type, TFNode* index, TFNode* val) {
  if (!graph) return nullptr;
  const compiler::Operator* op =
      graph->machine()->CheckedStore(MachineTypeFor(type));
  TFNode* heap_buffer = HeapBuffer();
  TFNode* heap_size = HeapSize();
  TFNode* node = graph->graph()->NewNode(op, heap_buffer, index, heap_size, val,
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
