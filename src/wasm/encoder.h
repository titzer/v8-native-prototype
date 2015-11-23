// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_ENCODER_H_
#define V8_WASM_ENCODER_H_

#include "src/signature.h"
#include "src/zone-containers.h"

#include "src/base/smart-pointers.h"

#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"
#include "src/wasm/wasm-result.h"

namespace v8 {
namespace internal {
namespace wasm {

class WasmModuleBuilder;

class WasmFunctionEncoder : public ZoneObject {
 public:
  uint32_t HeaderSize() const;
  uint32_t BodySize() const;
  void Serialize(byte* buffer, byte** header, byte** body) const;

 private:
  WasmFunctionEncoder(Zone* zone,
                      uint8_t return_type,
                      uint8_t exported,
                      uint8_t external);
  friend class WasmFunctionBuilder;
  uint16_t signature_index_;
  ZoneVector<uint8_t> params_;
  uint16_t local_int32_count_;
  uint16_t local_int64_count_;
  uint16_t local_float32_count_;
  uint16_t local_float64_count_;
  uint8_t exported_;
  uint8_t external_;
  ZoneVector<uint8_t> body_;

  bool HasLocals() const {
    return (local_int32_count_ + local_int64_count_ + local_float32_count_ +
            local_float64_count_) > 0;
  }
};

class WasmFunctionBuilder : public ZoneObject {
 public:
  uint16_t AddParam(uint8_t type);
  uint16_t AddLocal(uint8_t type);
  void ReturnType(uint8_t type);
  void AddBody(const byte* code, uint32_t code_size);
  void AddBody(const byte* code,
               uint32_t code_size,
               const uint32_t* local_indices,
               uint32_t indices_size);
  void AppendCode(const byte opcode, bool add_local_operand);
  uint32_t AppendEditableImmediate(const byte immediate);
  void EditImmediate(uint32_t index, const byte immediate);
  void Exported(uint8_t flag);
  void External(uint8_t flag);
  WasmFunctionEncoder* Build(Zone* zone, WasmModuleBuilder* mb) const;

 private:
  WasmFunctionBuilder(Zone* zone);
  friend class WasmModuleBuilder;
  struct Type;
  uint8_t return_type_;
  ZoneVector<Type> locals_;
  uint8_t exported_;
  uint8_t external_;
  ZoneVector<uint8_t> body_;
  ZoneVector<uint32_t> local_indices_;
  uint16_t AddVar(uint8_t type, bool param);
  void IndexVars(WasmFunctionEncoder* e, uint16_t* var_index) const;
};

class WasmDataSegmentEncoder : public ZoneObject {
 public:
  WasmDataSegmentEncoder(Zone* zone,
                         const byte* data,
                         uint32_t size,
                         uint32_t dest);
  uint32_t HeaderSize() const;
  uint32_t BodySize() const;
  void Serialize(byte* buffer, byte** header, byte** body) const;

 private:
  ZoneVector<byte> data_;
  uint32_t dest_;
};

class WasmModuleIndex : public ZoneObject {
 public:
  const byte* Begin() const { return begin_; }
  const byte* End() const { return end_; }

 private:
  friend class WasmModuleWriter;
  WasmModuleIndex(const byte* begin, const byte* end)
      : begin_(begin), end_(end) {}
  const byte* begin_;
  const byte* end_;
};

class WasmModuleWriter : public ZoneObject {
 public:
  WasmModuleIndex* WriteTo(Zone* zone) const;

 private:
  friend class WasmModuleBuilder;
  WasmModuleWriter(Zone* zone);
  ZoneVector<WasmFunctionEncoder*> functions_;
  ZoneVector<WasmDataSegmentEncoder*> data_segments_;
  ZoneVector<FunctionSig*> signatures_;
  ZoneVector<uint16_t> indirect_functions_;
};

class WasmModuleBuilder : public ZoneObject {
 public:
  WasmModuleBuilder(Zone* zone);
  uint16_t AddFunction();
  WasmFunctionBuilder* FunctionAt(size_t index);
  void AddDataSegment(WasmDataSegmentEncoder* data);
  uint16_t AddSignature(FunctionSig* sig);
  void AddIndirectFunction(uint16_t index);
  WasmModuleWriter* Build(Zone* zone);

 private:
  struct CompareFunctionSigs {
    int operator()(FunctionSig* a, FunctionSig* b);
  };
  typedef ZoneMap<FunctionSig*, uint16_t, CompareFunctionSigs> SignatureMap;

  Zone* zone_;
  ZoneVector<FunctionSig*> signatures_;
  ZoneVector<WasmFunctionBuilder*> functions_;
  ZoneVector<WasmDataSegmentEncoder*> data_segments_;
  ZoneVector<uint16_t> indirect_functions_;
  SignatureMap signature_map_;
};

std::vector<uint8_t> UnsignedLEB128From(uint32_t result);
}
}
}

#endif  // V8_WASM_ENCODER_H_
