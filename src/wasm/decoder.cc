
#include "src/v8.h"
#include "src/signature.h"
#include "src/zone-containers.h"

#include "src/wasm/decoder.h"
#include "src/wasm/wasm-opcodes.h"
#include "src/wasm/tf-builder.h"

namespace v8 {
namespace internal {
namespace wasm {

// The root of a decoded tree.
struct Tree {
  AstType type : 3;   // tree type.
  int count : 29;     // number of children.
  const byte* pc;     // start of the syntax tree.
  TFNode* node;       // node in the TurboFan graph.
  Tree* children[1];  // pointers to children.

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

  bool end() { return state == kControlEnd; }
  void Kill() {
    state = kControlEnd;
    locals = nullptr;
    control = nullptr;
    effect = nullptr;
  }
};

// An entry in the stack of blocks during decoding.
struct Block {
  SsaEnv* cont_env;
  SsaEnv* break_env;
};

// An entry in the stack of ifs during decoding.
struct IfEnv {
  SsaEnv* true_env;
  SsaEnv* false_env;
};

// A LR-parser strategy for decoding Wasm code that uses an explicit
// shift-reduce strategy with multiple internal stacks.
class LR_WasmDecoder {
 public:
  LR_WasmDecoder(Zone* zone, TFGraph* g)
      : zone_(zone),
        builder_(zone, g),
        trees_(zone),
        stack_(zone),
        blocks_(zone),
        ifs_(zone) {}

  Result Decode(FunctionEnv* function_env, const byte* pc, const byte* end) {
    CHECK(end >= pc);
    trees_.clear();
    stack_.clear();
    blocks_.clear();
    ifs_.clear();
    result_.error_code = kSuccess;
    result_.tree = nullptr;
    result_.pc = pc;
    result_.error_pc = nullptr;
    result_.error_msg = nullptr;
    result_.error_pt = nullptr;

    start_ = pc;
    pc_ = pc;
    limit_ = end;
    function_env_ = function_env;

    InitSsaEnv();
    DecodeFunctionBody();

    if (result_.error_msg == nullptr && !ssa_env_->end()) {
      AddImplicitReturnAtEnd();
    }

    if (result_.error_msg == nullptr) {
      if (trees_.size() == 0) {
        error(start_, "no trees created");
      } else if (trees_.size() > 1) {
        error(start_, "more than one tree created");
      } else {
        result_.tree = trees_[0];
      }
    }
    return result_;
  }

 private:
  static const size_t kErrorMsgSize = 128;

  Zone* zone_;
  TFBuilder builder_;
  const byte* start_;
  const byte* pc_;
  const byte* limit_;
  Result result_;

  SsaEnv* ssa_env_;
  FunctionEnv* function_env_;

  ZoneVector<Tree*> trees_;
  ZoneVector<Production> stack_;
  ZoneVector<Block> blocks_;
  ZoneVector<IfEnv> ifs_;

  void InitSsaEnv() {
    FunctionSig* sig = function_env_->sig;
    int param_count = static_cast<int>(sig->parameter_count());
    TFNode* start = builder_.Start(param_count);
    SsaEnv* ssa_env = Split(nullptr);
    int pos = 0;
    // Initialize parameters.
    for (int i = 0; i < param_count; i++) {
      ssa_env->locals[pos++] = builder_.Param(i, sig->GetParam(i));
    }
    // Initialize int32 locals.
    if (function_env_->local_int32_count > 0) {
      TFNode* zero = builder_.Int32Constant(0);
      for (unsigned i = 0; i < function_env_->local_int32_count; i++) {
        ssa_env->locals[pos++] = zero;
      }
    }
    // Initialize float32 locals.
    if (function_env_->local_float32_count > 0) {
      TFNode* zero = builder_.Float32Constant(0);
      for (unsigned i = 0; i < function_env_->local_float32_count; i++) {
        ssa_env->locals[pos++] = zero;
      }
    }
    // Initialize float64 locals.
    if (function_env_->local_float64_count > 0) {
      TFNode* zero = builder_.Float64Constant(0);
      for (unsigned i = 0; i < function_env_->local_float64_count; i++) {
        ssa_env->locals[pos++] = zero;
      }
    }
    DCHECK_EQ(function_env_->total_locals, pos);
    DCHECK_EQ(EnvironmentCount(), pos);
    ssa_env->control = start;
    ssa_env->effect = start;
    if (function_env_->module) {
      builder_.heap_start = function_env_->module->heap_start;
      builder_.heap_end = function_env_->module->heap_end;
    }
    SetEnv(ssa_env);
  }

  void Leaf(AstType type, TFNode* node = nullptr) {
    size_t size = sizeof(Tree);
    Tree* tree = reinterpret_cast<Tree*>(zone_->New(size));
    tree->type = type;
    tree->count = 0;
    tree->pc = pc_;
    tree->node = node;
    tree->children[0] = nullptr;
    Reduce(tree);
  }

  void Shift(AstType type, unsigned count) {
    if (count == 0) return Leaf(type);
    size_t size = sizeof(Tree) + (count - 1) * sizeof(Tree*);
    Tree* tree = reinterpret_cast<Tree*>(zone_->New(size));
    tree->type = type;
    tree->count = count;
    tree->pc = pc_;
    tree->node = nullptr;
    for (unsigned i = 0; i < count; i++) tree->children[i] = nullptr;
    stack_.push_back({tree, 0});
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

  // Decodes the body of a function, producing reduced trees into {result}.
  void DecodeFunctionBody() {
    if (pc_ >= limit_) return;  // Nothing to do.

    while (true) {
      if (ssa_env_->end()) {
        error(pc_, "unreachable code");
        return;
      }

      int len = 1;
      WasmOpcode opcode = static_cast<WasmOpcode>(*pc_);

      switch (opcode) {
        case kStmtNop:
          Leaf(kAstStmt);
          break;
        case kStmtSetLocal: {
          LocalIndexOperand(pc_);  // Check valid local index.
          Shift(kAstStmt, 1);
          len = 2;
          break;
        }
        case kStmtSetGlobal: {
          GlobalIndexOperand(pc_);  // Check valid global index
          Shift(kAstStmt, 1);
          len = 2;
          break;
        }
        case kStmtSetHeap: {
          MemAccessTypeOperand(pc_);  // Check valid heap type.
          Shift(kAstStmt, 2);
          len = 2;
          break;
        }
        case kStmtIf: {
          Shift(kAstStmt, 2);
          break;
        }
        case kStmtIfThen: {
          Shift(kAstStmt, 3);
          break;
        }
        case kStmtSwitch:  // fallthru
        case kStmtSwitchNf: {
          int length = Operand<uint8_t>(pc_);
          Shift(kAstStmt, length + 1);
          SsaEnv* cont_env = nullptr;
          SsaEnv* break_env = UnreachableEnv();
          blocks_.push_back({cont_env, break_env});
          len = 2;
          break;
        }
        case kStmtBlock: {
          int length = Operand<uint8_t>(pc_);
          if (length == 0) {
            Leaf(kAstStmt);
          } else {
            Shift(kAstStmt, length);
            SsaEnv* cont_env = nullptr;
            SsaEnv* break_env = UnreachableEnv();
            blocks_.push_back({cont_env, break_env});
          }
          len = 2;
          break;
        }
        case kStmtLoop: {
          int length = Operand<uint8_t>(pc_);
          if (length == 0) {
            Leaf(kAstStmt);  // TODO: that's an infinite loop.
          } else {
            Shift(kAstStmt, length);
            PrepareForLoop(ssa_env_);
            SsaEnv* cont_env = ssa_env_;
            ssa_env_ = Split(ssa_env_);
            ssa_env_->state = SsaEnv::kReached;
            SsaEnv* break_env = UnreachableEnv();
            blocks_.push_back({cont_env, break_env});
          }
          len = 2;
          break;
        }
        case kStmtContinue: {
          unsigned depth = Operand<uint8_t>(pc_);
          if (depth < blocks_.size()) {
            Block* block = &blocks_[blocks_.size() - depth - 1];
            if (block->cont_env) {
              Goto(ssa_env_, block->cont_env);
            } else {
              error(pc_, "improper continue to block");
            }
            ssa_env_->state = SsaEnv::kControlEnd;
          } else {
            error(pc_, "improperly nested continue");
          }
          Leaf(kAstStmt);
          len = 2;
          break;
        }
        case kStmtBreak: {
          unsigned depth = Operand<uint8_t>(pc_);
          if (depth < blocks_.size()) {
            Block* block = &blocks_[blocks_.size() - depth - 1];
            Goto(ssa_env_, block->break_env);
            ssa_env_->state = SsaEnv::kControlEnd;
          } else {
            error(pc_, "improperly nested break");
          }
          Leaf(kAstStmt);
          len = 2;
          break;
        }
        case kStmtReturn: {
          unsigned count = Operand<uint8_t>(pc_);
          if (count != function_env_->sig->return_count()) {
            error(pc_, "mismatched return value count");
          }
          Shift(kAstStmt, count);
          len = 2;
          break;
        }
        case kExprInt8Const: {
          int32_t value = Operand<int8_t>(pc_);
          Leaf(kAstInt32, builder_.Int32Constant(value));
          len = 2;
          break;
        }
        case kExprInt32Const: {
          int32_t value = Operand<int32_t>(pc_);
          Leaf(kAstInt32, builder_.Int32Constant(value));
          len = 5;
          break;
        }
        case kExprFloat32Const: {
          float value = Operand<float>(pc_);
          Leaf(kAstFloat32, builder_.Float32Constant(value));
          len = 5;
          break;
        }
        case kExprFloat64Const: {
          double value = Operand<double>(pc_);
          Leaf(kAstFloat64, builder_.Float64Constant(value));
          len = 9;
          break;
        }
        case kExprGetLocal: {
          unsigned index = LocalIndexOperand(pc_);
          // TODO: redundant validity check for local index.
          TFNode* val = function_env_->IsValidLocal(index)
                            ? ssa_env_->locals[index]
                            : builder_.Error();
          Leaf(function_env_->GetLocalType(index), val);
          len = 2;
          break;
        }
        case kExprGetGlobal: {
          unsigned index = GlobalIndexOperand(pc_);
          AstType type =
              AstTypeFor(function_env_->module->GetGlobalType(index));
          Leaf(type, builder_.GetGlobal(index));
          len = 2;
          break;
        }
        case kExprGetHeap: {
          AstType type = AstTypeFor(MemAccessTypeOperand(pc_));
          Shift(type, 1);
          len = 2;
          break;
        }
        case kExprCallFunction: {
          FunctionSig* sig = FunctionIndexOperand(pc_);
          if (sig) {
            AstType type = kAstInt32;
            if (sig->return_count() == 1) {
              type = sig->GetReturn();
            } else {
              error(pc_, "function call should return exactly 1 result");
            }
            Shift(type, static_cast<int>(sig->parameter_count()));
          } else {
            Leaf(kAstInt32);
          }
          len = 2;
          break;
        }
        case kExprCallIndirect: {
          FunctionSig* sig = FunctionTableIndexOperand(pc_);
          if (sig) {
            AstType type = kAstInt32;
            if (sig->return_count() == 1) {
              type = sig->GetReturn();
            } else {
              error(pc_, "function call should return exactly 1 result");
            }
            Shift(type, static_cast<int>(1 + sig->parameter_count()));
          } else {
            Leaf(kAstInt32);
          }
          len = 2;
          break;
        }
        case kExprTernary: {
          Shift(kAstInt32, 3);  // Result type is typeof(x) in {c ? x : y}.
          break;
        }
        case kExprComma: {
          Shift(kAstInt32, 2);  // Result type is typeof(y) in {x, y}.
          break;
        }
        case kExprBoolNot: {
          Shift(kAstInt32, 1);
          break;
        }
// === Binops that return int32 ========================================
#define DECLARE_SHIFT_CASE(name, opcode) case kExpr##name:  // fallthrough
          FOREACH_I_II_OPCODE(DECLARE_SHIFT_CASE)
          FOREACH_I_FF_OPCODE(DECLARE_SHIFT_CASE)
          FOREACH_I_DD_OPCODE(DECLARE_SHIFT_CASE)
          Shift(kAstInt32, 2);
          break;
#undef DECLARE_SHIFT_CASE
// =====================================================================

// === Binops that return float32 ======================================
#define DECLARE_SHIFT_CASE(name, opcode) case kExpr##name:  // fallthrough
          FOREACH_F_FF_OPCODE(DECLARE_SHIFT_CASE)
          Shift(kAstFloat32, 2);
          break;
#undef DECLARE_SHIFT_CASE
// =====================================================================

// === Binops that return float64 ======================================
#define DECLARE_SHIFT_CASE(name, opcode) case kExpr##name:  // fallthrough
          FOREACH_D_DD_OPCODE(DECLARE_SHIFT_CASE)
          Shift(kAstFloat64, 2);
          break;
#undef DECLARE_SHIFT_CASE
        // =====================================================================
        case kExprInt32FromFloat32:   // fallthrough
        case kExprInt32FromFloat64:   // fallthrough
        case kExprUint32FromFloat32:  // fallthrough
        case kExprUint32FromFloat64:  // fallthrough
          Shift(kAstInt32, 1);
          break;
        case kExprFloat64FromSInt32:   // fallthrough
        case kExprFloat64FromUInt32:   // fallthrough
        case kExprFloat64FromFloat32:  // fallthrough
          Shift(kAstFloat64, 1);
          break;
        case kExprFloat32FromSInt32:   // fallthrough
        case kExprFloat32FromUInt32:   // fallthrough
        case kExprFloat32FromFloat64:  // fallthrough
          Shift(kAstFloat32, 1);
          break;

        default:
          error(pc_, "Invalid opcode");
          return;
      }
      pc_ += len;
      if (pc_ >= limit_) {
        // End of code reached or exceeded.
        if (pc_ > limit_ && result_.error_pc != nullptr) {
          error(pc_, "Beyond end of code");
        }
        return;
      }
    }
  }

  void AddImplicitReturnAtEnd() {
    // TODO
  }

  void Reduce(Production* p) {
    WasmOpcode opcode = p->opcode();
    switch (opcode) {
      case kStmtSetLocal: {
        unsigned index = LocalIndexOperand(p->pc());
        Tree* val = p->last();
        if (function_env_->GetLocalType(index) == val->type) {
          ssa_env_->locals[index] = val->node;
        } else {
          error(p->pc(), "Typecheck failed in SetLocal", val->pc);
        }
        break;
      }
      case kStmtSetGlobal: {
        unsigned index = LocalIndexOperand(p->pc());
        Tree* val = p->last();
        AstType global =
            AstTypeFor(function_env_->module->GetGlobalType(index));
        if (global == val->type) {
          p->tree->node = builder_.SetGlobal(index, val->node);
        } else {
          error(p->pc(), "Typecheck failed in SetGlobal", val->pc);
        }
        break;
      }
      case kStmtSetHeap: {
        if (p->index == 1) {
          TypeCheckLast(p, kAstInt32);
        } else if (p->index == 2) {
          Tree* val = p->last();
          MemType type = MemAccessTypeOperand(p->pc());
          if (AstTypeFor(type) == val->type) {
            Tree* ival = p->tree->children[0];
            p->tree->node = builder_.SetHeap(type, ival->node, val->node);
          } else {
            error(p->pc(), "Typecheck failed in SetHeap", val->pc);
          }
        }
        break;
      }
      case kStmtSwitch:  // fallthru
      case kStmtSwitchNf: {
        TFNode* key = p->tree->children[0]->node;
        if (p->index == 1) {
          // Condition done. Build comparison for first case.
          TypeCheckLast(p, kAstInt32);
          ifs_.push_back({Split(ssa_env_), ssa_env_});
          IfEnv* env = &ifs_.back();
          TFNode* caseval = builder_.Int32Constant(0);
          TFNode* cond = builder_.Binop(kExprInt32Eq, key, caseval);
          builder_.Branch(cond, &env->true_env->control,
                          &env->false_env->control);
          SetEnv(env->true_env);
        } else {
          // Just finished a case.
          TypeCheckLast(p, kAstStmt);
          SsaEnv* fallthru = ssa_env_;
          IfEnv* env = &ifs_.back();
          if (!p->done()) {
            // Build comparison for next case.
            TFNode* caseval = builder_.Int32Constant(p->index - 1);
            TFNode* cond = builder_.Binop(kExprInt32Eq, key, caseval);
            SsaEnv* true_env = env->true_env = Split(env->false_env);
            SetEnv(env->false_env);
            builder_.Branch(cond, &true_env->control, &env->false_env->control);
            if (!fallthru->end()) {
              // StmtSwitch falls through to next case, StmtSwitchNf to the end.
              SsaEnv* next = p->opcode() == kStmtSwitch
                                 ? true_env
                                 : blocks_.back().break_env;
              Goto(fallthru, next);
            }
            SetEnv(true_env);
          } else {
            // Finished all cases.
            Block* last = &blocks_.back();
            Goto(env->false_env, last->break_env);
            if (!fallthru->end()) {
              // Handle fallthru from this case to the end.
              Goto(fallthru, last->break_env);
            }
            SetEnv(last->break_env);
            ifs_.pop_back();
            blocks_.pop_back();
          }
        }
        break;
      }
      case kStmtBlock:
      case kStmtLoop: {
        TypeCheckLast(p, kAstStmt);
        if (p->done()) {
          Block* last = &blocks_.back();
          if (!ssa_env_->end()) {
            Goto(ssa_env_,
                 opcode == kStmtLoop ? last->cont_env : last->break_env);
          }
          SetEnv(last->break_env);
          blocks_.pop_back();
        }
        break;
      }
      case kStmtIf: {
        if (p->index == 1) {
          // Condition done. Split environment for true branch.
          TypeCheckLast(p, kAstInt32);
          ifs_.push_back({Split(ssa_env_), ssa_env_});
          IfEnv* env = &ifs_.back();
          builder_.Branch(p->last()->node, &env->true_env->control,
                          &env->false_env->control);
          SetEnv(env->true_env);
        } else if (p->index == 2) {
          // True block done. Merge true and false environments.
          TypeCheckLast(p, kAstStmt);
          IfEnv* env = &ifs_.back();
          env->false_env->state = SsaEnv::kReached;
          if (!ssa_env_->end()) Goto(ssa_env_, env->false_env);
          SetEnv(env->false_env);
          ifs_.pop_back();
        }
        break;
      }
      case kStmtIfThen: {
        if (p->index == 1) {
          // Condition done. Split environment for true branch.
          TypeCheckLast(p, kAstInt32);
          ifs_.push_back({Split(ssa_env_), Split(ssa_env_)});
          IfEnv* env = &ifs_.back();
          builder_.Branch(p->last()->node, &env->true_env->control,
                          &env->false_env->control);
          SetEnv(env->true_env);
        } else if (p->index == 2) {
          // True block done. Switch to environment for false branch.
          TypeCheckLast(p, kAstStmt);
          IfEnv* env = &ifs_.back();
          SetEnv(env->false_env);
        } else if (p->index == 3) {
          // False block done. Switch to environment for merge.
          TypeCheckLast(p, kAstStmt);
          IfEnv* env = &ifs_.back();
          if (ssa_env_->end()) {
            SetEnv(env->true_env);
          } else {
            ssa_env_->state = SsaEnv::kReached;
            Goto(env->true_env, ssa_env_);
          }
          ifs_.pop_back();
        }
        break;
      }
      case kStmtReturn: {
        TypeCheckLast(p, function_env_->sig->GetReturn(p->index - 1));
        if (p->done()) {
          int count = p->tree->count;
          TFNode** buffer = builder_.Buffer(count);
          for (int i = 0; i < count; i++) {
            buffer[i] = p->tree->children[i]->node;
          }
          builder_.Return(count, buffer);
          ssa_env_->state = SsaEnv::kControlEnd;
        }
        break;
      }
      case kExprGetHeap: {
        TypeCheckLast(p, kAstInt32);
        MemType type = MemAccessTypeOperand(p->pc());
        p->tree->node = builder_.GetHeap(type, p->last()->node);
        break;
      }
      case kExprCallFunction: {
        FunctionSig* sig = FunctionIndexOperand(p->pc());
        TypeCheckLast(p, sig->GetParam(p->index - 1));
        if (p->done()) {
          unsigned count = p->tree->count + 1;
          TFNode** buffer = builder_.Buffer(count);
          unsigned index = Operand<uint8_t>(p->pc());
          buffer[0] = builder_.FunctionConstant(index);
          for (int i = 1; i < count; i++) {
            buffer[i] = p->tree->children[i - 1]->node;
          }
          p->tree->node = builder_.Call(count, buffer);
        }
        break;
      }
      case kExprCallIndirect: {
        FunctionSig* sig = FunctionTableIndexOperand(p->pc());
        if (p->index == 1) {
          TypeCheckLast(p, kAstInt32);
        } else {
          TypeCheckLast(p, sig->GetParam(p->index));
        }
        if (p->done()) {
          unsigned count = p->tree->count;
          TFNode** buffer = builder_.Buffer(count);
          unsigned index = Operand<uint8_t>(p->pc());
          buffer[0] =
              builder_.FunctionTableLookup(index, p->tree->children[0]->node);
          for (int i = 1; i < count; i++) {
            buffer[i] = p->tree->children[i]->node;
          }
          p->tree->node = builder_.Call(count, buffer);
        }
        break;
      }
      case kExprTernary: {
        // TODO(titzer): reduce duplication with kStmtIfThen.
        Tree* left = p->tree->children[1];
        Tree* right = p->tree->children[2];
        if (p->index == 1) {
          TypeCheckLast(p, kAstInt32);
          // TODO(titzer): technically SSA renaming shouldn't be necessary,
          // just different control and effect dependencies.
          ifs_.push_back({Split(ssa_env_), Split(ssa_env_)});
          IfEnv* env = &ifs_.back();
          builder_.Branch(p->last()->node, &env->true_env->control,
                          &env->false_env->control);
          SetEnv(env->true_env);
        } else if (p->index == 2) {
          // True expr done. Switch to environment for false branch.
          if (left->type == kAstStmt) {
            char* buffer = reinterpret_cast<char*>(zone_->New(kErrorMsgSize));
            snprintf(buffer, kErrorMsgSize,
                     "%s[%d] expected expression, found %s statement",
                     OpcodeName(p->opcode()), p->index - 1,
                     OpcodeName(p->last()->opcode()));
            error(p->pc(), buffer, p->last()->pc);
          }
          IfEnv* env = &ifs_.back();
          SetEnv(env->false_env);
        } else if (p->index == 3) {
          // False expr done. Switch to environment for merge.
          TypeCheckLast(p, left->type);
          IfEnv* env = &ifs_.back();
          if (ssa_env_->end()) {
            SetEnv(env->true_env);
          } else {
            ssa_env_->state = SsaEnv::kReached;
            Goto(env->true_env, ssa_env_);
          }
          ifs_.pop_back();
          // Create a phi for the value output.
          TFNode* a = left->node;
          TFNode* b = right->node;
          TFNode* result = a;
          if (a != b) {
            TFNode* vals[] = {a, b};
            result = builder_.Phi(left->type, 2, vals, *builder_.control);
          }
          p->tree->node = result;
          p->tree->type = left->type;
        }

        break;
      }
      case kExprComma: {
        if (p->done()) {
          // The type of the comma operator is the type of the last
          // expression.
          p->tree->type = p->last()->type;
          p->tree->node = p->last()->node;
        }
        break;
      }
      case kExprBoolNot: {
        TypeCheckLast(p, kAstInt32);
        if (p->done()) {
          p->tree->node = builder_.Unop(opcode, p->tree->children[0]->node);
        }
        break;
      }
// === Binops that take int32 ============================================
#define DECLARE_REDUCE_CASE(name, opcode) case kExpr##name:  // fallthrough
        FOREACH_I_II_OPCODE(DECLARE_REDUCE_CASE)
        TypeCheckLast(p, kAstInt32);
        if (p->done()) {
          p->tree->node = builder_.Binop(opcode, p->tree->children[0]->node,
                                         p->tree->children[1]->node);
        }
        break;
#undef DECLARE_REDUCE_CASE
// =======================================================================

// === Binops that take float32 ==========================================
#define DECLARE_REDUCE_CASE(name, opcode) case kExpr##name:  // fallthrough
        FOREACH_F_FF_OPCODE(DECLARE_REDUCE_CASE)
        FOREACH_I_FF_OPCODE(DECLARE_REDUCE_CASE)
        TypeCheckLast(p, kAstFloat32);
        if (p->done()) {
          p->tree->node = builder_.Binop(opcode, p->tree->children[0]->node,
                                         p->tree->children[1]->node);
        }
        break;
#undef DECLARE_REDUCE_CASE
// =======================================================================

// === Binops that take float64 ==========================================
#define DECLARE_REDUCE_CASE(name, opcode) case kExpr##name:  // fallthrough
        FOREACH_D_DD_OPCODE(DECLARE_REDUCE_CASE)
        FOREACH_I_DD_OPCODE(DECLARE_REDUCE_CASE)
        TypeCheckLast(p, kAstFloat64);
        if (p->done()) {
          p->tree->node = builder_.Binop(opcode, p->tree->children[0]->node,
                                         p->tree->children[1]->node);
        }
        break;
#undef DECLARE_REDUCE_CASE
      // =======================================================================
      case kExprInt32FromFloat64:    // fallthrough
      case kExprUint32FromFloat64:   // fallthrough
      case kExprFloat32FromFloat64:  // fallthrough
        TypeCheckLast(p, kAstFloat64);
        p->tree->node = builder_.Unop(opcode, p->tree->children[0]->node);
        break;
      case kExprFloat64FromSInt32:  // fallthrough
      case kExprFloat64FromUInt32:  // fallthrough
      case kExprFloat32FromSInt32:  // fallthrough
      case kExprFloat32FromUInt32:  // fallthrough
        TypeCheckLast(p, kAstInt32);
        p->tree->node = builder_.Unop(opcode, p->tree->children[0]->node);
        break;

      case kExprInt32FromFloat32:    // fallthrough
      case kExprUint32FromFloat32:   // fallthrough
      case kExprFloat64FromFloat32:  // fallthrough
        TypeCheckLast(p, kAstFloat32);
        p->tree->node = builder_.Unop(opcode, p->tree->children[0]->node);
        break;

      default:
        break;
    }
  }

  void TypeCheckLast(Production* p, AstType expected) {
    if (p->last()->type != expected) {
      char* buffer = reinterpret_cast<char*>(zone_->New(kErrorMsgSize));
      snprintf(buffer, kErrorMsgSize,
               "%s[%d] expected type %s, found %s of type %s",
               OpcodeName(p->opcode()), p->index - 1, TypeName(expected),
               OpcodeName(p->last()->opcode()), TypeName(p->last()->type));
      error(p->pc(), buffer, p->last()->pc);
    }
  }

  void SetEnv(SsaEnv* env) {
    ssa_env_ = env;
    builder_.control = &env->control;
    builder_.effect = &env->effect;
  }

  void Goto(SsaEnv* from, SsaEnv* to) {
    DCHECK_NOT_NULL(to);
    if (from->end()) return;
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
        TFNode* controls[] = {from->control, to->control};
        TFNode* merge = builder_.Merge(2, controls);
        to->control = merge;
        // Merge effects.
        if (from->effect != to->effect) {
          TFNode* effects[] = {from->effect, to->effect, merge};
          to->effect = builder_.EffectPhi(2, effects, merge);
        }
        // Merge SSA values.
        for (int i = EnvironmentCount() - 1; i >= 0; i--) {
          TFNode* a = from->locals[i];
          TFNode* b = to->locals[i];
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
          unsigned count = builder_.InputCount(merge);
          TFNode** effects = builder_.Buffer(count);
          for (int j = 0; j < count - 1; j++) effects[j] = to->effect;
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
            unsigned count = builder_.InputCount(merge);
            TFNode** vals = builder_.Buffer(count);
            for (int j = 0; j < count - 1; j++) vals[j] = tnode;
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

  void PrepareForLoop(SsaEnv* env) {
    env->state = SsaEnv::kMerged;
    env->control = builder_.Loop(env->control);
    env->effect = builder_.EffectPhi(1, &env->effect, env->control);
    for (int i = EnvironmentCount() - 1; i >= 0; i--) {
      env->locals[i] = builder_.Phi(function_env_->GetLocalType(i), 1,
                                    &env->locals[i], env->control);
    }
  }

  SsaEnv* Split(SsaEnv* from) {
    SsaEnv* result = reinterpret_cast<SsaEnv*>(zone_->New(sizeof(SsaEnv)));
    size_t size = sizeof(TFNode*) * function_env_->GetLocalCount();
    result->locals =
        size > 0 ? reinterpret_cast<TFNode**>(zone_->New(size)) : nullptr;
    if (from) {
      DCHECK(!from->end());
      memcpy(result->locals, from->locals, size);
      result->control = from->control;
      result->effect = from->effect;
    }
    result->state = SsaEnv::kReached;
    return result;
  }

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
    return static_cast<int>(function_env_->GetLocalCount());
  }

  unsigned LocalIndexOperand(const byte* pc) {
    unsigned index = Operand<uint8_t>(pc);
    if (!function_env_->IsValidLocal(index)) {
      error(pc, "invalid local variable index");
    }
    return index;
  }

  unsigned GlobalIndexOperand(const byte* pc) {
    unsigned index = Operand<uint8_t>(pc);
    if (!function_env_->module->IsValidGlobal(index)) {
      error(pc, "invalid global variable index");
    }
    return index;
  }

  FunctionSig* FunctionIndexOperand(const byte* pc) {
    unsigned index = Operand<uint8_t>(pc);
    FunctionSig* sig = function_env_->module->GetFunctionSignature(index);
    if (!sig) error(pc, "invalid function index");
    return sig;
  }

  FunctionSig* FunctionTableIndexOperand(const byte* pc) {
    unsigned index = Operand<uint8_t>(pc);
    FunctionSig* sig = function_env_->module->GetFunctionTableSignature(index);
    if (!sig) error(pc, "invalid function table index");
    return sig;
  }

  MemType MemAccessTypeOperand(const byte* pc) {
    MemType type = static_cast<MemType>(Operand<uint8_t>(pc));
    switch (type) {
      case kMemInt8:
      case kMemUint8:
      case kMemInt16:
      case kMemUint16:
      case kMemInt32:
      case kMemUint32:
      case kMemFloat32:
      case kMemFloat64:
        return type;
      default:
        error(pc, "invalid type for heap access");
        return kMemInt32;
    }
  }

  AstType AstTypeFor(MemType type) {
    switch (type) {
      case kMemInt8:
      case kMemUint8:
      case kMemInt16:
      case kMemUint16:
      case kMemInt32:
      case kMemUint32:
        return kAstInt32;
      case kMemFloat32:
        return kAstFloat32;
      case kMemFloat64:
        return kAstFloat64;
    }
  }

  void error(const byte* pc, const char* msg, const byte* pt = nullptr) {
    limit_ = start_;  // terminates the decoding loop
    if (result_.error_msg == nullptr) {
      result_.error_code = kError;  // TODO error code
      result_.error_msg = msg;
      result_.error_pc = pc;
      result_.error_pt = pt;
#if DEBUG
      PrintStackForDebugging();
#endif
    }
  }

#if DEBUG
  void PrintStackForDebugging() { PrintProduction(0); }

  void PrintProduction(size_t depth) {
    if (depth >= stack_.size()) return;
    Production* p = &stack_[depth];
    for (size_t d = 0; d < depth; d++) PrintF("  ");
    PrintF("@%d %s [%d]\n", static_cast<int>(p->tree->pc - start_),
           OpcodeName(p->opcode()), p->tree->count);
    for (int i = 0; i < p->index; i++) {
      Tree* child = p->tree->children[i];
      for (size_t d = 0; d <= depth; d++) PrintF("  ");
      PrintF("@%d %s [%d]", static_cast<int>(child->pc - start_),
             OpcodeName(child->opcode()), child->count);
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


Result VerifyWasmCode(FunctionEnv* env, const byte* start, const byte* end) {
  Zone zone;
  LR_WasmDecoder decoder(&zone, nullptr);
  Result result = decoder.Decode(env, start, end);
  return result;
}


Result BuildTFGraph(TFGraph* graph, FunctionEnv* env, const byte* start,
                    const byte* end) {
  Zone zone;
  LR_WasmDecoder decoder(&zone, graph);
  Result result = decoder.Decode(env, start, end);
  return result;
}


void TestWasmDecodingSpeed() {
  byte code[] = {kStmtSetLocal, 0, kExprInt32Add, kExprGetLocal, 0,
                 kExprInt8Const, 5};

  Zone zone;
  LR_WasmDecoder decoder(&zone, nullptr);
  FunctionSig::Builder builder(&zone, 1, 1);
  builder.AddReturn(kAstInt32);
  builder.AddParam(kAstInt32);
  FunctionEnv env = {nullptr, builder.Build(), 0, 0, 0, 0};

  // Make COUNT copies of the above code.
  const int TRIALS = 10;
  const int COUNT = (4 * 1024) / sizeof(code);
  const int TOTAL = COUNT * sizeof(code);
  byte* big_code = reinterpret_cast<byte*>(zone.New(TOTAL));
  for (int i = 0; i < COUNT; i++) {
    memcpy(big_code + i * sizeof(code), code, sizeof(code));
  }

  for (int i = 0; i < TRIALS; i++) {
    base::ElapsedTimer timer;
    timer.Start();
    Result result = decoder.Decode(&env, big_code, big_code + TOTAL);
    int64_t us = timer.Elapsed().InMicroseconds();
    OFStream os(stdout);
    double rate = ((TOTAL * 1000000.0) / us) / 1048576;
    os << result << TOTAL << " bytes, us: " << us << " (" << rate << " MB/sec)"
       << std::endl;
  }
}


std::ostream& operator<<(std::ostream& os, const Tree& tree) {
  if (tree.pc == nullptr) {
    os << "null";
    return os;
  }
  PrintF("%s", OpcodeName(tree.opcode()));
  if (tree.count > 0) os << "(";
  for (int i = 0; i < tree.count; i++) {
    if (i > 0) os << ", ";
    os << *tree.children[i];
  }
  if (tree.count > 0) os << ")";
  return os;
}


std::ostream& operator<<(std::ostream& os, const Result& result) {
  os << "Result = ";
  if (result.error_msg != nullptr) {
    os << result.error_msg << " @+"
       << static_cast<ptrdiff_t>(result.error_pc - result.pc);
  } else if (result.tree != nullptr) {
    os << *result.tree;
  } else {
    os << "null";
  }
  os << std::endl;
  return os;
}


std::ostream& operator<<(std::ostream& os, const ErrorCode& error_code) {
  switch (error_code) {
    case kSuccess:
      os << "Success";
      break;
    default:  // TODO: render error codes
      os << "Error";
      break;
  }
  return os;
}
}
}
}
