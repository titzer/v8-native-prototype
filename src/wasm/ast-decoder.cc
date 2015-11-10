// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/base/platform/elapsed-timer.h"
#include "src/signature.h"

#include "src/zone-containers.h"
#include "src/flags.h"
#include "src/handles.h"

#include "src/wasm/ast-decoder.h"
#include "src/wasm/tf-builder.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"

namespace v8 {
namespace internal {
namespace wasm {

#if DEBUG
#define TRACE(...)               \
  do {                           \
    if (FLAG_trace_wasm_decoder) \
      PrintF(__VA_ARGS__);       \
  } while (false)
#else
#define TRACE(...)
#endif

// The root of a decoded tree.
struct Tree {
  LocalType type : 4;  // tree type.
  int count : 28;      // number of children.
  const byte* pc;      // start of the syntax tree.
  TFNode* node;        // node in the TurboFan graph.
  Tree* children[1];   // pointers to children.

  WasmOpcode opcode() const { return static_cast<WasmOpcode>(*pc); }
};

// A production represents an incomplete decoded tree in the LR decoder.
struct Production {
  Tree* tree;  // the root of the syntax tree.
  int index;   // the current index into the children of the tree.

  WasmOpcode opcode() const { return static_cast<WasmOpcode>(*pc()); }
  const byte* pc() const { return tree->pc; }
  bool done() const { return index >= tree->count; }
  Tree* last() const { return index > 0 ? tree->children[index - 1] : nullptr; }
};

// An SsaEnv environment carries the current local variable renaming
// as well as the current effect and control dependency in the TF graph.
struct SsaEnv {
  enum State { kControlEnd, kUnreachable, kReached, kMerged };

  State state;
  TFNode* control;
  TFNode* effect;
  TFNode** locals;

  bool go() { return state == kReached || state == kMerged; }
  void Kill(State new_state = kControlEnd) {
    state = new_state;
    locals = nullptr;
    control = nullptr;
    effect = nullptr;
  }
};

// An entry in the stack of blocks during decoding.
struct Block {
  SsaEnv* ssa_env;  // SSA renaming environment.
  int stack_depth;  // production stack depth.
};

// An entry in the stack of ifs during decoding.
struct IfEnv {
  SsaEnv* true_env;
  SsaEnv* false_env;
};

// A shift-reduce-parser strategy for decoding Wasm code that uses an explicit
// shift-reduce strategy with multiple internal stacks.
class LR_WasmDecoder : public Decoder {
 public:
  LR_WasmDecoder(Zone* zone, TFGraph* g)
      : Decoder(nullptr, nullptr),
        zone_(zone),
        builder_(zone, g),
        trees_(zone),
        stack_(zone),
        blocks_(zone),
        ifs_(zone) {}

  TreeResult Decode(FunctionEnv* function_env,
                    const byte* base,
                    const byte* pc,
                    const byte* end) {
    base::ElapsedTimer decode_timer;
    if (FLAG_trace_wasm_decode_time) {
      decode_timer.Start();
    }
    trees_.clear();
    stack_.clear();
    blocks_.clear();
    ifs_.clear();

    if (end < pc) {
      error(pc, "function body end < start");
      return result_;
    }

    base_ = base;
    Reset(pc, end);
    function_env_ = function_env;

    InitSsaEnv();
    DecodeFunctionBody();

    Tree* tree = nullptr;
    if (ok()) {
      if (ssa_env_->go()) {
	if (stack_.size() > 0) {
	  error(stack_.back().pc(), end, "fell off end of code");
	}
	AddImplicitReturnAtEnd();
      }
      if (trees_.size() == 0) {
	if (function_env_->sig->return_count() > 0) {
	  error(start_, "no trees created");
	}
      } else {
	tree = trees_[0];
      }
    }

    if (ok()) {
      if (FLAG_trace_wasm_decode_time) {
        double ms = decode_timer.Elapsed().InMillisecondsF();
        PrintF(" - decoding took %0.3f ms\n", ms);
      }
      TRACE("wasm-decode ok\n\n");
    } else {
      TRACE("wasm-error module+%-6d func+%d: %s\n\n", baserel(error_pc_),
            startrel(error_pc_), error_msg_.get());
    }
    return toResult(tree);
  }

 private:
  static const size_t kErrorMsgSize = 128;

  Zone* zone_;
  TFBuilder builder_;
  const byte* base_;
  TreeResult result_;

  SsaEnv* ssa_env_;
  FunctionEnv* function_env_;

  ZoneVector<Tree*> trees_;
  ZoneVector<Production> stack_;
  ZoneVector<Block> blocks_;
  ZoneVector<IfEnv> ifs_;

  void InitSsaEnv() {
    FunctionSig* sig = function_env_->sig;
    int param_count = static_cast<int>(sig->parameter_count());
    TFNode* start = builder_.Start(param_count + 1);
    SsaEnv* ssa_env = Split(nullptr);
    int pos = 0;
    if (builder_.graph) {
      // Initialize parameters.
      for (int i = 0; i < param_count; i++) {
        ssa_env->locals[pos++] = builder_.Param(i, sig->GetParam(i));
      }
      // Initialize int32 locals.
      if (function_env_->local_int32_count > 0) {
        TFNode* zero = builder_.Int32Constant(0);
        for (uint32_t i = 0; i < function_env_->local_int32_count; i++) {
          ssa_env->locals[pos++] = zero;
        }
      }
      // Initialize int64 locals.
      if (function_env_->local_int64_count > 0) {
        TFNode* zero = builder_.Int64Constant(0);
        for (uint32_t i = 0; i < function_env_->local_int64_count; i++) {
          ssa_env->locals[pos++] = zero;
        }
      }
      // Initialize float32 locals.
      if (function_env_->local_float32_count > 0) {
        TFNode* zero = builder_.Float32Constant(0);
        for (uint32_t i = 0; i < function_env_->local_float32_count; i++) {
          ssa_env->locals[pos++] = zero;
        }
      }
      // Initialize float64 locals.
      if (function_env_->local_float64_count > 0) {
        TFNode* zero = builder_.Float64Constant(0);
        for (uint32_t i = 0; i < function_env_->local_float64_count; i++) {
          ssa_env->locals[pos++] = zero;
        }
      }
      DCHECK_EQ(function_env_->total_locals, pos);
      DCHECK_EQ(EnvironmentCount(), pos);
    }
    ssa_env->control = start;
    ssa_env->effect = start;
    builder_.module = function_env_->module;
    SetEnv(ssa_env);
  }

  void Leaf(LocalType type, TFNode* node = nullptr) {
    size_t size = sizeof(Tree);
    Tree* tree = reinterpret_cast<Tree*>(zone_->New(size));
    tree->type = type;
    tree->count = 0;
    tree->pc = pc_;
    tree->node = node;
    tree->children[0] = nullptr;
    Reduce(tree);
  }

  void Shift(LocalType type, uint32_t count) {
    size_t size =
        sizeof(Tree) + (count == 0 ? 0 : ((count - 1) * sizeof(Tree*)));
    Tree* tree = reinterpret_cast<Tree*>(zone_->New(size));
    tree->type = type;
    tree->count = count;
    tree->pc = pc_;
    tree->node = nullptr;
    for (uint32_t i = 0; i < count; i++)
      tree->children[i] = nullptr;
    if (count == 0) {
      Production p = {tree, 0};
      Reduce(&p);
      Reduce(tree);
    } else {
      stack_.push_back({tree, 0});
    }
  }

  void Reduce(Tree* tree) {
    while (true) {
      if (stack_.size() == 0) {
        trees_.push_back(tree);
        break;
      }
      Production* p = &stack_.back();
      p->tree->children[p->index++] = tree;
      Reduce(p);
      if (p->done()) {
        tree = p->tree;
        stack_.pop_back();
      } else {
        break;
      }
    }
  }

  char* indentation() {
    static const int kMaxIndent = 64;
    static char bytes[kMaxIndent + 1];
    for (int i = 0; i < kMaxIndent; i++)
      bytes[i] = ' ';
    bytes[kMaxIndent] = 0;
    if (stack_.size() < kMaxIndent / 2) {
      bytes[stack_.size() * 2] = 0;
    }
    return bytes;
  }

  // Decodes the body of a function, producing reduced trees into {result}.
  void DecodeFunctionBody() {
    TRACE("wasm-decode %p...%p (%d bytes) %s\n",
          reinterpret_cast<const void*>(start_),
          reinterpret_cast<const void*>(limit_),
          static_cast<int>(limit_ - start_),
          builder_.graph ? "graph building" : "");

    if (pc_ >= limit_)
      return;  // Nothing to do.

    while (true) {  // decoding loop.
      if (!ssa_env_->go()) {
        error("unreachable code");
        return;
      }

      int len = 1;
      WasmOpcode opcode = static_cast<WasmOpcode>(*pc_);
      TRACE("wasm-decode module+%-6d %s func+%d: 0x%02x %s\n", baserel(pc_),
            indentation(), startrel(pc_), opcode,
            WasmOpcodes::OpcodeName(opcode));

      FunctionSig* sig = WasmOpcodes::Signature(opcode);
      if (sig) {
        // A simple expression with a fixed signature.
        Shift(sig->GetReturn(), static_cast<uint32_t>(sig->parameter_count()));
        pc_ += len;
        if (pc_ >= limit_) {
          // End of code reached or exceeded.
          if (pc_ > limit_ && ok()) {
            error("Beyond end of code");
          }
          return;
        }
        continue;  // back to decoding loop.
      }

      switch (opcode) {
        case kExprNop:
          Leaf(kAstStmt);
          break;
        case kExprBlock: {
          int length = Operand<uint8_t>(pc_);
          if (length < 1) {
            Leaf(kAstStmt);
          } else {
            Shift(kAstStmt, length);
	    // The break environment is the outer environment.
	    SsaEnv* break_env = ssa_env_;
	    PushBlock(break_env);
	    SetEnv(Steal(break_env));
          }
          len = 2;
          break;
        }
        case kExprLoop: {
          int length = Operand<uint8_t>(pc_);
          if (length < 1) {
	    Leaf(kAstStmt);
          } else {
            Shift(kAstStmt, length);
	    // The break environment is the outer environment.
	    SsaEnv* break_env = ssa_env_;
	    PushBlock(break_env);
	    SsaEnv* cont_env = Steal(break_env);
	    // The continue environment is the inner environment.
            PrepareForLoop(cont_env);
            SetEnv(Split(cont_env));
            ssa_env_->state = SsaEnv::kReached;
	    PushBlock(cont_env);
          }
          len = 2;
          break;
        }
        case kExprIf:
          Shift(kAstStmt, 2);
          break;
        case kExprIfThen:
          Shift(kAstStmt, 3);  // Result type is typeof(x) in {c ? x : y}.
          break;
        case kExprSelect:
          Shift(kAstStmt, 3);  // Result type is typeof(x) in {c ? x : y}.
          break;
        case kExprBr: {
          uint32_t depth = Operand<uint8_t>(pc_);
          Shift(kAstStmt, 1);
          if (depth >= blocks_.size()) {
	    error("improperly nested branch");
          }
          len = 2;
          break;
        }
        case kExprBrIf: {
          uint32_t depth = Operand<uint8_t>(pc_);
          Shift(kAstStmt, 2);
          if (depth >= blocks_.size()) {
            error("improperly nested conditional branch");
          }
          len = 2;
          break;
        }
        case kExprTableSwitch: {
	  error("tableswitch unimplemented");
	  break;
        }
        case kExprReturn: {
	  error("return unimplemented");
	  break;
        }
        case kExprUnreachable: {
	  Leaf(kAstStmt, nullptr);
	  builder_.Unreachable();
	  ssa_env_->Kill();
	  break;
        }
        case kExprI8Const: {
          int32_t value = Operand<int8_t>(pc_);
          Leaf(kAstI32, builder_.Int32Constant(value));
          len = 2;
          break;
        }
        case kExprI32Const: {
          int32_t value = Operand<int32_t>(pc_);
          Leaf(kAstI32, builder_.Int32Constant(value));
          len = 5;
          break;
        }
        case kExprI64Const: {
          int64_t value = Operand<int64_t>(pc_);
          Leaf(kAstI64, builder_.Int64Constant(value));
          len = 9;
          break;
        }
        case kExprF32Const: {
          float value = Operand<float>(pc_);
          Leaf(kAstF32, builder_.Float32Constant(value));
          len = 5;
          break;
        }
        case kExprF64Const: {
          double value = Operand<double>(pc_);
          Leaf(kAstF64, builder_.Float64Constant(value));
          len = 9;
          break;
        }
        case kExprGetLocal: {
          uint32_t index;
          LocalType type = LocalOperand(pc_, &index, &len);
          TFNode* val = builder_.graph && type != kAstStmt
                            ? ssa_env_->locals[index]
                            : builder_.Error();
          Leaf(type, val);
          break;
        }
        case kExprSetLocal: {
          uint32_t index;
          LocalType type = LocalOperand(pc_, &index, &len);
          Shift(type, 1);
          break;
        }
        case kExprLoadGlobal: {
          uint32_t index;
          LocalType type = GlobalOperand(pc_, &index, &len);
          Leaf(type, builder_.LoadGlobal(index));
          break;
        }
        case kExprStoreGlobal: {
          uint32_t index;
          LocalType type = GlobalOperand(pc_, &index, &len);
          Shift(type, 1);
          break;
        }
        case kExprI32LoadMemL:  // fallthru
        case kExprI32LoadMemH: {
          MemAccessTypeOperand(pc_, kAstI32);  // check width.
          Shift(kAstI32, 1);
          len = 2;
          break;
        }
        case kExprI64LoadMemL:  // fallthru.
        case kExprI64LoadMemH: {
          MemAccessTypeOperand(pc_, kAstI64);  // check width.
          Shift(kAstI64, 1);
          len = 2;
          break;
        }
        case kExprF32LoadMemL:  // fallthru.
        case kExprF32LoadMemH:
          MemAccessTypeOperand(pc_, kAstF32);  // check width.
          Shift(kAstF32, 1);
          len = 2;
          break;
        case kExprF64LoadMemL:  // fallthru.
        case kExprF64LoadMemH:
          MemAccessTypeOperand(pc_, kAstF64);  // check width.
          Shift(kAstF64, 1);
          len = 2;
          break;
        case kExprI32StoreMemL:  // fallthru.
        case kExprI32StoreMemH: {
          MemAccessTypeOperand(pc_, kAstI32);  // check width.
          Shift(kAstI32, 2);
          len = 2;
          break;
        }
        case kExprI64StoreMemL:  // fallthru.
        case kExprI64StoreMemH: {
          MemAccessTypeOperand(pc_, kAstI64);  // check width.
          Shift(kAstI64, 2);
          len = 2;
          break;
        }
        case kExprF32StoreMemL:  // fallthru.
        case kExprF32StoreMemH:
          MemAccessTypeOperand(pc_, kAstF32);  // check width.
          Shift(kAstF32, 2);
          len = 2;
          break;
        case kExprF64StoreMemL:  // fallthru.
        case kExprF64StoreMemH:
          MemAccessTypeOperand(pc_, kAstF64);  // check width.
          Shift(kAstF64, 2);
          len = 2;
          break;
        case kExprPageSize:
          // TODO(titzer): is this the correct constant for all platforms?
          Leaf(kAstI32, builder_.Int32Constant(
                            static_cast<int32_t>(base::OS::CommitPageSize())));
          break;
        case kExprMemorySize:
          Leaf(kAstI32, builder_.MemSize());
          break;
        case kExprResizeMemL:
          Shift(kAstI32, 1);
          break;
        case kExprResizeMemH:
          Shift(kAstI64, 1);
          break;
        case kExprCallFunction: {
          uint32_t unused;
          FunctionSig* sig = FunctionSigOperand(pc_, &unused, &len);
          if (sig) {
            LocalType type =
                sig->return_count() == 0 ? kAstStmt : sig->GetReturn();
            Shift(type, static_cast<int>(sig->parameter_count()));
          } else {
            Leaf(kAstI32);  // error
          }
          break;
        }
        case kExprCallIndirect: {
          uint32_t unused;
          FunctionSig* sig = SigOperand(pc_, &unused, &len);
          if (sig) {
            LocalType type = kAstI32;
            if (sig->return_count() == 1) {
              type = sig->GetReturn();
            } else {
              error("function call should return exactly 1 result");
            }
            Shift(type, static_cast<int>(1 + sig->parameter_count()));
          } else {
            Leaf(kAstI32);  // error
          }
          break;
        }
        default:
          error("Invalid opcode");
          return;
      }
      pc_ += len;
      if (pc_ >= limit_) {
        // End of code reached or exceeded.
        if (pc_ > limit_ && ok()) {
          error("Beyond end of code");
        }
        return;
      }
    }
  }

  void PushBlock(SsaEnv* ssa_env) {
    blocks_.push_back({ssa_env, static_cast<int>(stack_.size() - 1)});
  }

  Tree* GetLastValueIfBlock(Tree* tree) {
    return tree;   // XXX TODO
  }

  void AddImplicitReturnAtEnd() {
    int retcount = static_cast<int>(function_env_->sig->return_count());
    if (retcount == 0)
      return builder_.ReturnVoid();

    if (trees_.size() < function_env_->sig->return_count()) {
      error(limit_, nullptr,
            "ImplicitReturn expects %d arguments, only %d remain", retcount,
            static_cast<int>(trees_.size()));
      return;
    }

    TRACE("wasm-decode implicit return of %d args\n", retcount);

    TFNode** buffer = builder_.Buffer(retcount);
    for (int index = 0; index < retcount; index++) {
      Tree* tree = GetLastValueIfBlock(trees_[trees_.size() - 1 - index]);
      buffer[index] = tree->node;
      LocalType expected = function_env_->sig->GetReturn(index);
      if (tree->type != expected) {
        error(limit_, tree->pc,
              "ImplicitReturn[%d] expected type %s, found %s of type %s", index,
              WasmOpcodes::TypeName(expected),
              WasmOpcodes::OpcodeName(tree->opcode()),
              WasmOpcodes::TypeName(tree->type));
        return;
      }
    }

    builder_.Return(retcount, buffer);
  }

  int baserel(const byte* ptr) {
    return base_ ? static_cast<int>(ptr - base_) : 0;
  }

  int startrel(const byte* ptr) { return static_cast<int>(ptr - start_); }

  void Reduce(Production* p) {
    WasmOpcode opcode = p->opcode();
    TRACE("-----reduce module+%-6d %s func+%d: 0x%02x %s\n", baserel(p->pc()),
          indentation(), startrel(p->pc()), opcode,
          WasmOpcodes::OpcodeName(opcode));
    FunctionSig* sig = WasmOpcodes::Signature(opcode);
    if (sig) {
      // A simple expression with a fixed signature.
      TypeCheckLast(p, sig->GetParam(p->index - 1));
      if (p->done()) {
        if (sig->parameter_count() == 2) {
          p->tree->node = builder_.Binop(opcode, p->tree->children[0]->node,
                                         p->tree->children[1]->node);
        } else if (sig->parameter_count() == 1) {
          p->tree->node = builder_.Unop(opcode, p->tree->children[0]->node);
        } else {
          UNREACHABLE();
        }
      }
      return;
    }

    switch (opcode) {
      case kExprBlock: {
        if (p->done()) {
          Block* last = &blocks_.back();
	  DCHECK_EQ(stack_.size() - 1, last->stack_depth);
          if (ssa_env_->go()) {
            // fallthrough with the last expression.
            ReduceBreakToExprBlock(p, last);
          }
          SetEnv(last->ssa_env);
          blocks_.pop_back();
        }
        break;
      }
      case kExprLoop: {
        if (p->done()) {
	  // Pop the continue environment.
          blocks_.pop_back();
	  // Get the break environment.
          Block* last = &blocks_.back();
	  DCHECK_EQ(stack_.size() - 1, last->stack_depth);
          if (ssa_env_->go()) {
            // fallthrough with the last expression.
            ReduceBreakToExprBlock(p, last);
          }
          SetEnv(last->ssa_env);
          blocks_.pop_back();
        }
        break;
      }
      case kExprIf: {
        if (p->index == 1) {
          // Condition done. Split environment for true branch.
          TypeCheckLast(p, kAstI32);
          ifs_.push_back({Split(ssa_env_), ssa_env_});
          IfEnv* env = &ifs_.back();
          builder_.Branch(p->last()->node, &env->true_env->control,
                          &env->false_env->control);
          SetEnv(env->true_env);
        } else if (p->index == 2) {
          // True block done. Merge true and false environments.
          IfEnv* env = &ifs_.back();
	  SsaEnv* merge = env->false_env;
          if (ssa_env_->go() && merge->go()) {
	    merge->state = SsaEnv::kReached;
            Goto(ssa_env_, merge);
	  }
          SetEnv(merge);
          ifs_.pop_back();
        }
        break;
      }
      case kExprIfThen: {
        Tree* left = p->tree->children[1];
        Tree* right = p->tree->children[2];
        if (p->index == 1) {
          TypeCheckLast(p, kAstI32);
          ifs_.push_back({Split(ssa_env_), ssa_env_});
          IfEnv* env = &ifs_.back();
          builder_.Branch(p->last()->node, &env->true_env->control,
                          &env->false_env->control);
          SetEnv(env->true_env);
        } else if (p->index == 2) {
          // True expr done.
	  // Switch to environment for false branch.
	  SsaEnv* false_env = ifs_.back().false_env;
	  if (false_env->go()) false_env->state = SsaEnv::kReached;
          SetEnv(false_env);
        } else if (p->index == 3) {
          // False expr done.
          IfEnv* env = &ifs_.back();
          if (ssa_env_->go()) {
            ssa_env_->state = SsaEnv::kReached;
            if (env->true_env->go()) {
	      // Merge true env into (end of) false env.
	      Goto(env->true_env, ssa_env_);
	      if (right->type == left->type && left->type != kAstStmt) {
		// Create a phi for the value output.
		TFNode* a = left->node;
		TFNode* b = right->node;
		TFNode* result = a;
		if (a != b) {
		  TFNode* vals[] = {b, a};  // false predecessor first, then true.
		  result = builder_.Phi(left->type, 2, vals, *builder_.control);
		}
		p->tree->type = left->type;
		p->tree->node = result;
	      }
	    } else {
	      // Only false environment fell through.
	    }
          } else if (env->true_env->go()) {
	    // Only true environment fell through.
	    ssa_env_->Kill(SsaEnv::kUnreachable);
	    Goto(env->true_env, ssa_env_);
	  } else {
	    // Neither environment fell through.
	    ssa_env_->Kill(SsaEnv::kUnreachable);
          }
          ifs_.pop_back();
        }
        break;
      }
      case kExprSelect: {
        if (p->index == 1) {
	  // Condition done.
          TypeCheckLast(p, kAstI32);
        } else if (p->index == 2) {
	  // True expression done.
	  p->tree->type = p->last()->type;
	  if (p->tree->type == kAstStmt) {
	    error(p->pc(), p->tree->children[1]->pc,
		  "select operand should be expression");
	  }
        } else {
	  // False expression done.
	  DCHECK(p->done());
	  TypeCheckLast(p, p->tree->type);
	  if (ssa_env_->go()) {
	    TFNode* controls[2];
	    builder_.Branch(p->tree->children[0]->node, &controls[0], &controls[1]);
	    TFNode* merge = builder_.Merge(2, controls);
	    TFNode* vals[2] = {p->tree->children[1]->node, p->tree->children[2]->node};
	    TFNode* phi = builder_.Phi(p->tree->type, 2, vals, merge);
	    p->tree->node = phi;
	    ssa_env_->control = merge;
	  }
	}
	break;
      }
      case kExprBr: {
        uint32_t depth = Operand<uint8_t>(p->pc());
        if (depth >= blocks_.size()) {
          error("improperly nested branch");
	  break;
	}
	Block* block = &blocks_[blocks_.size() - depth - 1];
	ReduceBreakToExprBlock(p, block);
        break;
      }
      case kExprBrIf: {
        uint32_t depth = Operand<uint8_t>(p->pc());
        if (depth >= blocks_.size()) {
          error("improperly nested branch");
        } else if (ssa_env_->go() && p->done()) {
	  Block* block = &blocks_[blocks_.size() - depth - 1];
	  SsaEnv* fenv = ssa_env_;
	  SsaEnv* tenv = Split(fenv);
          builder_.Branch(p->tree->children[0]->node, &tenv->control, &fenv->control);
	  ssa_env_ = tenv;
	  ReduceBreakToExprBlock(p, block);
	  ssa_env_ = fenv;
	}
        break;
      }
      case kExprTableSwitch: {
	error("tableswitch unimplemented");
	break;
      }
      case kExprReturn: {
	error("return unimplemented");
	break;
      }
      case kExprSetLocal: {
        int unused = 0;
        uint32_t index;
        LocalType type = LocalOperand(p->pc(), &index, &unused);
        Tree* val = p->last();
        if (type == val->type) {
          if (builder_.graph)
           ssa_env_->locals[index] = val->node;
          p->tree->node = val->node;
        } else {
          error(p->pc(), val->pc, "Typecheck failed in SetLocal");
        }
        break;
      }
      case kExprStoreGlobal: {
        int unused = 0;
        uint32_t index;
        LocalType type = GlobalOperand(p->pc(), &index, &unused);
        Tree* val = p->last();
        if (type == val->type) {
          builder_.StoreGlobal(index, val->node);
          p->tree->node = val->node;
        } else {
          error(p->pc(), val->pc, "Typecheck failed in StoreGlobal");
        }
        break;
      }

      case kExprI32LoadMemL:
        return ReduceLoadMem(p, false, kAstI32);
      case kExprI32LoadMemH:
        return ReduceLoadMem(p, true, kAstI32);
      case kExprI64LoadMemL:
        return ReduceLoadMem(p, false, kAstI64);
      case kExprI64LoadMemH:
        return ReduceLoadMem(p, true, kAstI64);
      case kExprF32LoadMemL:
        return ReduceLoadMem(p, false, kAstF32);
      case kExprF32LoadMemH:
        return ReduceLoadMem(p, true, kAstF32);
      case kExprF64LoadMemL:
        return ReduceLoadMem(p, false, kAstF64);
      case kExprF64LoadMemH:
        return ReduceLoadMem(p, true, kAstF64);

      case kExprI32StoreMemL:
        return ReduceStoreMem(p, false, kAstI32);
      case kExprI32StoreMemH:
        return ReduceStoreMem(p, true, kAstI32);
      case kExprI64StoreMemL:
        return ReduceStoreMem(p, false, kAstI64);
      case kExprI64StoreMemH:
        return ReduceStoreMem(p, true, kAstI64);
      case kExprF32StoreMemL:
        return ReduceStoreMem(p, false, kAstF32);
      case kExprF32StoreMemH:
        return ReduceStoreMem(p, true, kAstF32);
      case kExprF64StoreMemL:
        return ReduceStoreMem(p, false, kAstF64);
      case kExprF64StoreMemH:
        return ReduceStoreMem(p, true, kAstF64);
      case kExprResizeMemL:
        TypeCheckLast(p, kAstI32);
        // TODO: build node for ResizeMemL
        p->tree->node = builder_.Int32Constant(0);
        return;
      case kExprResizeMemH:
        TypeCheckLast(p, kAstI64);
        // TODO: build node for ResizeMemH
        p->tree->node = builder_.Int64Constant(0);
        return;

      case kExprCallFunction: {
        int len;
        uint32_t index;
        FunctionSig* sig = FunctionSigOperand(p->pc(), &index, &len);
        if (!sig)
          break;
        if (p->index > 0) {
          TypeCheckLast(p, sig->GetParam(p->index - 1));
        }
        if (p->done()) {
          uint32_t count = p->tree->count + 1;
          TFNode** buffer = builder_.Buffer(count);
          FunctionSig* sig = FunctionSigOperand(p->pc(), &index, &len);
          USE(sig);
          buffer[0] = nullptr;  // reserved for code object.
          for (int i = 1; i < count; i++) {
            buffer[i] = p->tree->children[i - 1]->node;
          }
          p->tree->node = builder_.CallDirect(index, buffer);
        }
        break;
      }
      case kExprCallIndirect: {
        int len;
        uint32_t index;
        FunctionSig* sig = SigOperand(p->pc(), &index, &len);
        if (p->index == 1) {
          TypeCheckLast(p, kAstI32);
        } else {
          TypeCheckLast(p, sig->GetParam(p->index - 2));
        }
        if (p->done()) {
          uint32_t count = p->tree->count;
          TFNode** buffer = builder_.Buffer(count);
          for (int i = 0; i < count; i++) {
            buffer[i] = p->tree->children[i]->node;
          }
          p->tree->node = builder_.CallIndirect(index, buffer);
        }
        break;
      }
      default:
        break;
    }
  }

  void ReduceBreakToExprBlock(Production* p, Block* block) {
    Production* bp = &stack_[block->stack_depth];
    Tree* expr = p->last();
    if (block->ssa_env->state == SsaEnv::kUnreachable) {
      // first break out of this block; set the type and the node.
      Goto(ssa_env_, block->ssa_env);
      bp->tree->type = expr->type;
      bp->tree->node = expr->node;
    } else {
      // merge with the existing value for this block.
      Goto(ssa_env_, block->ssa_env);
      LocalType type = bp->tree->type;
      if (expr->type != type) {
	type = kAstStmt;
	p->tree->type = kAstStmt;
	p->tree->node = nullptr;
      } else if (type != kAstStmt) {
	bp->tree->node = CreateOrMergeIntoPhi(type, block->ssa_env->control,
					      bp->tree->node, expr->node);
      }
    }
  }

  void ReduceLoadMem(Production* p, bool high, LocalType type) {
    TypeCheckLast(p, high ? kAstI64 : kAstI32);  // index
    MemType mem_type = MemAccessTypeOperand(p->pc(), type);
    p->tree->node = builder_.LoadMem(type, mem_type, p->last()->node);
  }

  void ReduceStoreMem(Production* p, bool high, LocalType type) {
    if (p->index == 1) {
      TypeCheckLast(p, high ? kAstI64 : kAstI32);  // index
    } else if (p->index == 2) {
      TypeCheckLast(p, type);
      MemType mem_type = MemAccessTypeOperand(p->pc(), type);
      TFNode* val = p->tree->children[1]->node;
      builder_.StoreMem(mem_type, p->tree->children[0]->node, val);
      p->tree->node = val;
    }
  }

  void TypeCheckLast(Production* p, LocalType expected) {
    if (p->last()->type != expected && expected != kAstStmt) {
      error(p->pc(), p->last()->pc,
            "%s[%d] expected type %s, found %s of type %s",
            WasmOpcodes::OpcodeName(p->opcode()), p->index - 1,
            WasmOpcodes::TypeName(expected),
            WasmOpcodes::OpcodeName(p->last()->opcode()),
            WasmOpcodes::TypeName(p->last()->type));
    }
  }

  void SetEnv(SsaEnv* env) {
    TRACE("  env = %p (%d)\n", static_cast<void*>(env), env->state);
    ssa_env_ = env;
    builder_.control = &env->control;
    builder_.effect = &env->effect;
  }

  void Goto(SsaEnv* from, SsaEnv* to) {
    DCHECK_NOT_NULL(to);
    if (!from->go()) return;
    switch (to->state) {
      case SsaEnv::kUnreachable: {  // Overwrite destination.
        to->state = SsaEnv::kReached;
        to->locals = from->locals;
        to->control = from->control;
        to->effect = from->effect;
        break;
      }
      case SsaEnv::kReached: {  // Create a new merge.
        to->state = SsaEnv::kMerged;
        // Merge control.
        TFNode* controls[] = {to->control, from->control};
        TFNode* merge = builder_.Merge(2, controls);
        to->control = merge;
        // Merge effects.
        if (from->effect != to->effect) {
          TFNode* effects[] = {to->effect, from->effect, merge};
          to->effect = builder_.EffectPhi(2, effects, merge);
        }
        // Merge SSA values.
        for (int i = EnvironmentCount() - 1; i >= 0; i--) {
          TFNode* a = to->locals[i];
          TFNode* b = from->locals[i];
          if (a != b) {
            TFNode* vals[] = {a, b};
            to->locals[i] =
                builder_.Phi(function_env_->GetLocalType(i), 2, vals, merge);
          }
        }
        break;
      }
      case SsaEnv::kMerged: {
        TFNode* merge = to->control;
        // Extend the existing merge.
        builder_.AppendToMerge(merge, from->control);
        // Merge effects.
        if (builder_.IsPhiWithMerge(to->effect, merge)) {
          builder_.AppendToPhi(merge, to->effect, from->effect);
        } else if (to->effect != from->effect) {
          uint32_t count = builder_.InputCount(merge);
          TFNode** effects = builder_.Buffer(count);
          for (int j = 0; j < count - 1; j++)
            effects[j] = to->effect;
          effects[count - 1] = from->effect;
          to->effect = builder_.EffectPhi(count, effects, merge);
        }
        // Merge locals.
        for (int i = EnvironmentCount() - 1; i >= 0; i--) {
          TFNode* tnode = to->locals[i];
          TFNode* fnode = from->locals[i];
          if (builder_.IsPhiWithMerge(tnode, merge)) {
            builder_.AppendToPhi(merge, tnode, fnode);
          } else if (tnode != fnode) {
            uint32_t count = builder_.InputCount(merge);
            TFNode** vals = builder_.Buffer(count);
            for (int j = 0; j < count - 1; j++)
              vals[j] = tnode;
            vals[count - 1] = fnode;
            to->locals[i] = builder_.Phi(function_env_->GetLocalType(i), count,
                                         vals, merge);
          }
        }
        break;
      }
      default:
        UNREACHABLE();
    }
    return from->Kill();
  }

  TFNode* CreateOrMergeIntoPhi(LocalType type,
                               TFNode* merge,
                               TFNode* tnode,
                               TFNode* fnode) {
    if (!builder_.graph)
      return nullptr;
    if (builder_.IsPhiWithMerge(tnode, merge)) {
      builder_.AppendToPhi(merge, tnode, fnode);
    } else if (tnode != fnode) {
      uint32_t count = builder_.InputCount(merge);
      TFNode** vals = builder_.Buffer(count);
      for (int j = 0; j < count - 1; j++)
        vals[j] = tnode;
      vals[count - 1] = fnode;
      return builder_.Phi(type, count, vals, merge);
    }
    return tnode;
  }

  void BuildInfiniteLoop() {
    PrepareForLoop(ssa_env_);
    SsaEnv* cont_env = ssa_env_;
    ssa_env_ = Split(ssa_env_);
    ssa_env_->state = SsaEnv::kReached;
    Goto(ssa_env_, cont_env);
  }

  void PrepareForLoop(SsaEnv* env) {
    env->state = SsaEnv::kMerged;
    env->control = builder_.Loop(env->control);
    env->effect = builder_.EffectPhi(1, &env->effect, env->control);
    builder_.Terminate(env->effect, env->control);
    for (int i = EnvironmentCount() - 1; i >= 0; i--) {
      env->locals[i] = builder_.Phi(function_env_->GetLocalType(i), 1,
                                    &env->locals[i], env->control);
    }
  }

  // Create a complete copy of the {from}.
  SsaEnv* Split(SsaEnv* from) {
    SsaEnv* result = reinterpret_cast<SsaEnv*>(zone_->New(sizeof(SsaEnv)));
    size_t size = sizeof(TFNode*) * EnvironmentCount();
    result->locals =
        size > 0 ? reinterpret_cast<TFNode**>(zone_->New(size)) : nullptr;
    if (from) {
      DCHECK(from->go());
      memcpy(result->locals, from->locals, size);
      result->control = from->control;
      result->effect = from->effect;
      result->state = from->state == SsaEnv::kUnreachable ? SsaEnv::kUnreachable
                                                          : SsaEnv::kReached;
    } else {
      result->state = SsaEnv::kReached;
    }
    return result;
  }

  // Create a copy of {from} that steals its state and leaves {from}
  // unreachable.
  SsaEnv* Steal(SsaEnv* from) {
    SsaEnv* result = reinterpret_cast<SsaEnv*>(zone_->New(sizeof(SsaEnv)));
    if (from) {
      DCHECK(from->go());
      result->locals = from->locals;
      result->control = from->control;
      result->effect = from->effect;
      result->state = from->state == SsaEnv::kUnreachable ? SsaEnv::kUnreachable
       : SsaEnv::kReached;
      from->Kill(SsaEnv::kUnreachable);
    } else {
      result->state = SsaEnv::kReached;
      result->locals = nullptr;
    }
    return result;
  }

  // Create an unreachable environment.
  SsaEnv* UnreachableEnv() {
    SsaEnv* result = reinterpret_cast<SsaEnv*>(zone_->New(sizeof(SsaEnv)));
    result->state = SsaEnv::kUnreachable;
    result->control = nullptr;
    result->effect = nullptr;
    result->locals = nullptr;
    return result;
  }

  // Load an operand at [pc + 1].
  template <typename V>
  V Operand(const byte* pc) {
    if ((limit_ - pc) < static_cast<int>(1 + sizeof(V))) {
      const char* msg = "Expected operand following opcode";
      switch (sizeof(V)) {
        case 1:
          msg = "Expected 1-byte operand following opcode";
          break;
        case 2:
          msg = "Expected 2-byte operand following opcode";
          break;
        case 4:
          msg = "Expected 4-byte operand following opcode";
          break;
        default:
          break;
      }
      error(pc, msg);
      return -1;
    }
    return *reinterpret_cast<const V*>(pc + 1);
  }

  int EnvironmentCount() {
    if (builder_.graph)
      return static_cast<int>(function_env_->GetLocalCount());
    return 0;  // if we aren't building a graph, don't bother with SSA renaming.
  }

  LocalType LocalOperand(const byte* pc, uint32_t* index, int* length) {
    *index = UnsignedLEB128Operand(pc, length);
    if (function_env_->IsValidLocal(*index)) {
      return function_env_->GetLocalType(*index);
    }
    error(pc, "invalid local variable index");
    return kAstStmt;
  }

  LocalType GlobalOperand(const byte* pc, uint32_t* index, int* length) {
    *index = UnsignedLEB128Operand(pc, length);
    if (function_env_->module->IsValidGlobal(*index)) {
      return WasmOpcodes::LocalTypeFor(
          function_env_->module->GetGlobalType(*index));
    }
    error(pc, "invalid global variable index");
    return kAstStmt;
  }

  FunctionSig* FunctionSigOperand(const byte* pc,
                                  uint32_t* index,
                                  int* length) {
    *index = UnsignedLEB128Operand(pc, length);
    if (function_env_->module->IsValidFunction(*index)) {
      return function_env_->module->GetFunctionSignature(*index);
    }
    error(pc, "invalid function index");
    return nullptr;
  }

  FunctionSig* SigOperand(const byte* pc, uint32_t* index, int* length) {
    *index = UnsignedLEB128Operand(pc, length);
    if (function_env_->module->IsValidSignature(*index)) {
      return function_env_->module->GetSignature(*index);
    }
    error(pc, "invalid signature index");
    return nullptr;
  }

  uint32_t UnsignedLEB128Operand(const byte* pc, int* length) {
    uint32_t result = 0;
    ReadUnsignedLEB128ErrorCode error_code =
        ReadUnsignedLEB128Operand(pc + 1, limit_, length, &result);
    if (error_code == kInvalidLEB128)
      error(pc, "invalid LEB128 varint");
    if (error_code == kMissingLEB128)
      error(pc, "expected LEB128 varint");
    (*length)++;
    return result;
  }

  MemType MemAccessTypeOperand(const byte* pc, LocalType type) {
    byte operand = Operand<uint8_t>(pc);
    if (type == kAstF32)
      return kMemF32;
    if (type == kAstF64)
      return kMemF64;
    bool is64 = type == kAstI64;
    bool signext = MemoryAccess::SignExtendField::decode(operand);
    if (operand &
        ~(MemoryAccess::SignExtendField::kMask |
          MemoryAccess::IntWidthField::kMask)) {
      error(pc, "unrecognized bits in memory access operand");
      return kMemI32;
    }
    switch (MemoryAccess::IntWidthField::decode(operand)) {
      case MemoryAccess::kI8:
        return signext ? kMemI8 : kMemU8;
      case MemoryAccess::kI16:
        return signext ? kMemI16 : kMemU16;
      case MemoryAccess::kI32:
        return signext ? kMemI32 : kMemU32;
      case MemoryAccess::kI64:
        if (is64)
          return signext ? kMemI64 : kMemU64;
      default:
        error(pc, "invalid width for int memory access");
        return kMemI32;
    }
  }

  virtual void onFirstError() {
    limit_ = start_;           // Terminate decoding loop.
    builder_.graph = nullptr;  // Don't build any more nodes.
#if DEBUG
    PrintStackForDebugging();
#endif
  }

#if DEBUG
  void PrintStackForDebugging() { PrintProduction(0); }

  void PrintProduction(size_t depth) {
    if (depth >= stack_.size())
      return;
    Production* p = &stack_[depth];
    for (size_t d = 0; d < depth; d++) PrintF("  ");

    PrintF("@%d %s [%d]\n", static_cast<int>(p->tree->pc - start_),
           WasmOpcodes::OpcodeName(p->opcode()), p->tree->count);
    for (int i = 0; i < p->index; i++) {
      Tree* child = p->tree->children[i];
      for (size_t d = 0; d <= depth; d++) PrintF("  ");
      PrintF("@%d %s [%d]", static_cast<int>(child->pc - start_),
             WasmOpcodes::OpcodeName(child->opcode()), child->count);
      if (child->node) {
        PrintF(" => TF");
        TFBuilder::PrintDebugName(child->node);
      }
      PrintF("\n");
    }
    PrintProduction(depth + 1);
  }
#endif
};

TreeResult VerifyWasmCode(FunctionEnv* env,
                          const byte* base,
                          const byte* start,
                          const byte* end) {
  Zone zone;
  LR_WasmDecoder decoder(&zone, nullptr);
  TreeResult result = decoder.Decode(env, base, start, end);
  return result;
}

TreeResult BuildTFGraph(TFGraph* graph,
                        FunctionEnv* env,
                        const byte* base,
                        const byte* start,
                        const byte* end) {
  Zone zone;
  LR_WasmDecoder decoder(&zone, graph);
  TreeResult result = decoder.Decode(env, base, start, end);
  return result;
}

std::ostream& operator<<(std::ostream& os, const Tree& tree) {
  if (tree.pc == nullptr) {
    os << "null";
    return os;
  }
  PrintF("%s", WasmOpcodes::OpcodeName(tree.opcode()));
  if (tree.count > 0)
    os << "(";
  for (int i = 0; i < tree.count; i++) {
    if (i > 0)
      os << ", ";
    os << *tree.children[i];
  }
  if (tree.count > 0)
    os << ")";
  return os;
}

ReadUnsignedLEB128ErrorCode ReadUnsignedLEB128Operand(const byte* pc,
                                                      const byte* limit,
                                                      int* length,
                                                      uint32_t* result) {
  *result = 0;
  const byte* ptr = pc;
  const byte* end = pc + 5;  // maximum 5 bytes.
  if (end > limit)
    end = limit;
  int shift = 0;
  byte b = 0;
  while (ptr < end) {
    b = *ptr++;
    *result = *result | ((b & 0x7F) << shift);
    if ((b & 0x80) == 0)
      break;
    shift += 7;
  }
  DCHECK_LE(ptr - pc, 5);
  *length = static_cast<int>(ptr - pc);
  if (ptr == end && (b & 0x80)) {
    return kInvalidLEB128;
  } else if (*length == 0) {
    return kMissingLEB128;
  } else {
    return kNoError;
  }
}

int OpcodeLength(const byte* pc) {
  switch (static_cast<WasmOpcode>(*pc)) {
#define DECLARE_OPCODE_CASE(name, opcode, sig) case kExpr##name:
    FOREACH_LOAD_MEM_OPCODE(DECLARE_OPCODE_CASE)
    FOREACH_STORE_MEM_OPCODE(DECLARE_OPCODE_CASE)
#undef DECLARE_OPCODE_CASE

    case kExprI8Const:
    case kExprBlock:
    case kExprLoop:
    case kExprBr:
    case kExprBrIf:
      return 2;
    case kExprI32Const:
    case kExprF32Const:
      return 5;
    case kExprI64Const:
    case kExprF64Const:
      return 9;
    case kExprStoreGlobal:
    case kExprSetLocal:
    case kExprLoadGlobal:
    case kExprCallFunction:
    case kExprCallIndirect:
    case kExprGetLocal: {
      int length;
      uint32_t result = 0;
      ReadUnsignedLEB128Operand(pc + 1, pc + 6, &length, &result);
      return 1 + length;
    }

    default:
      return 1;
  }
}

int OpcodeArity(FunctionEnv* env, const byte* pc) {
#define DECLARE_ARITY(name, ...)                          \
  static const LocalType kTypes_##name[] = {__VA_ARGS__}; \
  static const int kArity_##name =                        \
      static_cast<int>(arraysize(kTypes_##name) - 1);

  FOREACH_SIGNATURE(DECLARE_ARITY);
#undef DECLARE_ARITY

  switch (static_cast<WasmOpcode>(*pc)) {
    case kExprI8Const:
    case kExprI32Const:
    case kExprI64Const:
    case kExprF64Const:
    case kExprF32Const:
    case kExprGetLocal:
    case kExprLoadGlobal:
    case kExprNop:
    case kExprUnreachable: 
      return 0;

    case kExprBr:
    case kExprStoreGlobal:
    case kExprSetLocal:
      return 1;

    case kExprIf:
    case kExprBrIf:
      return 2;
    case kExprIfThen:
    case kExprSelect:
      return 3;
    case kExprBlock:
    case kExprLoop:
      return *(pc + 1);

    case kExprCallFunction: {
      int index = *(pc + 1);
      return static_cast<int>(
          env->module->GetFunctionSignature(index)->parameter_count());
    }
    case kExprCallIndirect: {
      int index = *(pc + 1);
      return 1 + static_cast<int>(
                     env->module->GetSignature(index)->parameter_count());
    }
    case kExprReturn: 
      return static_cast<int>(env->sig->return_count());
    case kExprTableSwitch:
      UNIMPLEMENTED();
      return 0;

#define DECLARE_OPCODE_CASE(name, opcode, sig) \
  case kExpr##name:                            \
    return kArity_##sig;

      FOREACH_LOAD_MEM_OPCODE(DECLARE_OPCODE_CASE)
      FOREACH_STORE_MEM_OPCODE(DECLARE_OPCODE_CASE)
      FOREACH_MISC_MEM_OPCODE(DECLARE_OPCODE_CASE)
      FOREACH_SIMPLE_OPCODE(DECLARE_OPCODE_CASE)
#undef DECLARE_OPCODE_CASE
  }
}
}
}
}
