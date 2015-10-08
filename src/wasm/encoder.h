// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#ifndef V8_WASM_ENCODER_H_
#define V8_WASM_ENCODER_H_

#include "src/signature.h"
#include "src/zone-containers.h"

#include "src/base/smart-pointers.h"

#include "src/wasm/wasm-opcodes.h"
#include "src/wasm/wasm-result.h"

namespace v8 {
namespace internal {
namespace wasm {

//TODO: name all variables
class WasmFunctionEncoder : public ZoneObject {
 public:
  uint32_t HeaderSize(void) const;
  uint32_t BodySize(void) const;
  void Serialize(byte*, uint32_t, uint32_t) const;

  ~WasmFunctionEncoder();

 private:
  WasmFunctionEncoder(Zone* z, uint8_t return_type, uint8_t exported,
                      uint8_t external);
  friend class WasmFunctionBuilder;
  uint8_t return_type_;
  ZoneVector<uint8_t> params_;
  uint16_t local_int32_count_;
  uint16_t local_int64_count_;
  uint16_t local_float32_count_;
  uint16_t local_float64_count_;
  uint8_t exported_;
  uint8_t external_;
  ZoneVector<uint8_t> body_;
  void SerializeFunctionHeader(byte* buffer, uint32_t header_begin,
                               uint32_t body_begin) const;
  void SerializeFunctionBody(byte* buffer, uint32_t body_begin) const;
};

class WasmFunctionBuilder : public ZoneObject {
 public:
  uint16_t AddParam(uint8_t);
  uint16_t AddLocal(uint8_t);
  void ReturnType(uint8_t);
  void AddBody(const byte*, uint32_t);
  void AddBody(const byte*, uint32_t, const uint32_t*, uint32_t);
  void AppendCode(const byte opcode, bool add_local_operand);
  void Exported(uint8_t);
  void External(uint8_t);
  WasmFunctionEncoder* Build(Zone*) const;

  ~WasmFunctionBuilder();

 private:
  WasmFunctionBuilder(Zone*);
  friend class WasmModuleBuilder;
  struct Type;
  uint8_t return_type_;
  ZoneVector<Type> locals_;
  uint8_t exported_;
  uint8_t external_;
  ZoneVector<uint8_t> body_;
  ZoneVector<uint32_t> local_indices_;
  uint16_t AddVar(uint8_t, bool);
  void IndexVars(WasmFunctionEncoder*, uint16_t*) const;
};

class WasmDataSegmentEncoder : public ZoneObject {
 public:
  WasmDataSegmentEncoder(Zone*, const byte*, uint32_t, uint32_t);
  uint32_t HeaderSize() const;
  uint32_t BodySize() const;
  void Serialize(byte*, uint32_t, uint32_t) const;

  ~WasmDataSegmentEncoder();

 private:
  ZoneVector<byte> data_;
  uint32_t dest_;
};

class WasmModuleIndex : public ZoneObject {
 public:
  const byte* Begin() const { return begin_; }
  const byte* End() const { return end_; }

  ~WasmModuleIndex();

 private:
  friend class WasmModuleWriter;
  WasmModuleIndex(const byte* begin, const byte* end)
      : begin_(begin), end_(end) {}
  const byte* begin_;
  const byte* end_;
};

class WasmModuleWriter : public ZoneObject {
 public:
  WasmModuleIndex* WriteTo(Zone*) const;

  ~WasmModuleWriter();

 private:
  friend class WasmModuleBuilder;
  WasmModuleWriter(Zone*);
  ZoneVector<WasmFunctionEncoder*> functions_;
  ZoneVector<WasmDataSegmentEncoder*> data_segments_;
};

class WasmModuleBuilder : public ZoneObject {
 public:
  WasmModuleBuilder(Zone*);
  uint16_t AddFunction();
  WasmFunctionBuilder* FunctionAt(uint8_t);
  void AddDataSegment(WasmDataSegmentEncoder*);
  WasmModuleWriter* Build(Zone*) const;

  ~WasmModuleBuilder();

 private:
  Zone* zone_;
  ZoneVector<WasmFunctionBuilder*> functions_;
  ZoneVector<WasmDataSegmentEncoder*> data_segments_;
};

std::vector<uint8_t> UnsignedLEB128From(uint32_t);

}
}
}


#endif  // V8_WASM_ENCODER_H_
