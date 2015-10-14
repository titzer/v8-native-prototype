// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/signature.h"

#include "src/v8.h"
#include "src/handles.h"
#include "src/zone-containers.h"

#include "src/wasm/decoder.h"
#include "src/wasm/encoder.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"

#include "src/v8memory.h"

namespace v8 {
namespace internal {
namespace wasm {

/*TODO: add error cases for adding too many locals, too many functions and bad
  indices in body */

namespace {
void EmitUint8(byte** b, uint8_t x) {
  Memory::uint8_at(*b) = x;
  *b += 1;
}

void EmitUint16(byte** b, uint16_t x) {
  Memory::uint16_at(*b) = x;
  *b += 2;
}

void EmitUint32(byte** b, uint32_t x) {
  Memory::uint32_at(*b) = x;
  *b += 4;
}
}

struct WasmFunctionBuilder::Type {
  bool param_;
  uint8_t type_;
};

WasmFunctionBuilder::WasmFunctionBuilder(Zone* z)
    : return_type_(kAstI32),
      locals_(z),
      exported_(0),
      external_(0),
      body_(z),
      local_indices_(z) {}

WasmFunctionBuilder::~WasmFunctionBuilder() {}

uint16_t WasmFunctionBuilder::AddParam(uint8_t type) {
  return AddVar(type, true);
}

uint16_t WasmFunctionBuilder::AddLocal(uint8_t type) {
  return AddVar(type, false);
}

uint16_t WasmFunctionBuilder::AddVar(uint8_t type, bool param) {
  Type t;
  t.param_ = param;
  t.type_ = type;
  locals_.push_back(t);
  return static_cast<uint16_t>(locals_.size() - 1);
}

void WasmFunctionBuilder::ReturnType(uint8_t type) { return_type_ = type; }

void WasmFunctionBuilder::AddBody(const byte* code, uint32_t code_size) {
  AddBody(code, code_size, NULL, 0);
}

void WasmFunctionBuilder::AddBody(
    const byte* code,
    uint32_t code_size,
    const uint32_t* local_indices,
    uint32_t indices_size) {
  size_t size = body_.size();
  for (size_t i = 0; i < code_size; i++) {
    body_.push_back(code[i]);
  }
  for (size_t i = 0; i < indices_size; i++) {
    local_indices_.push_back(local_indices[i] + static_cast<uint32_t>(size));
  }
}

void WasmFunctionBuilder::AppendCode(
    const byte opcode, bool add_local_operand) {
  body_.push_back(opcode);
  if (add_local_operand) {
    local_indices_.push_back(static_cast<uint32_t>(body_.size()) - 1);
  }
}

void WasmFunctionBuilder::Exported(uint8_t flag) { exported_ = flag; }

void WasmFunctionBuilder::External(uint8_t flag) { external_ = flag; }

WasmFunctionEncoder* WasmFunctionBuilder::Build(Zone* z) const {
  WasmFunctionEncoder* e = new (z) WasmFunctionEncoder(
      z, return_type_, exported_, external_);
  auto var_index = new uint16_t[locals_.size()];
  IndexVars(e, var_index);
  const byte* start = body_.data();
  const byte* end = start + body_.size();
  size_t local_index = 0;
  for(size_t i = 0; i < body_.size();) {
    if (local_index < local_indices_.size() &&
        i == local_indices_[local_index]) {
      int length = 0;
      uint32_t index;
      ReadUnsignedLEB128Operand(start+i, end, &length, &index);
      uint16_t new_index = var_index[index];
      const std::vector<uint8_t>& index_vec = UnsignedLEB128From(new_index);
      for(size_t j = 0; j < index_vec.size(); j++) {
        e->body_.push_back(index_vec.at(j));
      }
      i += length;
      local_index++;
    } else {
      e->body_.push_back(*(start+i));
      i++;
    }
  }
  delete[] var_index;
  return e;
}

void WasmFunctionBuilder::IndexVars(
    WasmFunctionEncoder* e,
    uint16_t* var_index) const {
  uint16_t param = 0;
  uint16_t int32 = 0;
  uint16_t int64 = 0;
  uint16_t float32 = 0;
  uint16_t float64 = 0;
  for(size_t i = 0; i < locals_.size(); i++) {
    if (locals_.at(i).param_) {
      param++;
    } else if (locals_.at(i).type_ == kAstI32) {
      int32++;
    } else if (locals_.at(i).type_ == kAstI64) {
      int64++;
    } else if (locals_.at(i).type_ == kAstF32) {
      float32++;
    } else if (locals_.at(i).type_ == kAstF64) {
      float64++;
    }
  }
  e->local_int32_count_ = int32;
  e->local_int64_count_ = int64;
  e->local_float32_count_ = float32;
  e->local_float64_count_ = float64;
  float64 = param + int32 + int64 + float32;
  float32 = param + int32 + int64;
  int64 = param + int32;
  int32 = param;
  param = 0;
  for(size_t i = 0; i < locals_.size(); i++) {
    if (locals_.at(i).param_) {
      e->params_.push_back(locals_.at(i).type_);
      var_index[i] = param++;
    } else if (locals_.at(i).type_ == kAstI32) {
      var_index[i] = int32++;
    } else if (locals_.at(i).type_ == kAstI64) {
      var_index[i] = int64++;
    } else if (locals_.at(i).type_ == kAstF32) {
      var_index[i] = float32++;
    } else if (locals_.at(i).type_ == kAstF64) {
      var_index[i] = float64++;
    }
  }
}

WasmFunctionEncoder::WasmFunctionEncoder(
    Zone* z, uint8_t return_type, uint8_t exported, uint8_t external)
    : return_type_(return_type),
      params_(z),
      exported_(exported),
      external_(external),
      body_(z) {}

WasmFunctionEncoder::~WasmFunctionEncoder() {}

uint32_t WasmFunctionEncoder::HeaderSize() const {
  static const uint32_t kMinFunctionSize = 24;
  return kMinFunctionSize + static_cast<uint32_t>(params_.size());
}

uint32_t WasmFunctionEncoder::BodySize(void) const {
  return static_cast<uint32_t>(body_.size());
}

void WasmFunctionEncoder::Serialize(byte* buffer, uint32_t header_begin,
                                    uint32_t body_begin) const {
  SerializeFunctionHeader(buffer, header_begin, body_begin);
  SerializeFunctionBody(buffer, body_begin);
}

void WasmFunctionEncoder::SerializeFunctionHeader(byte* buffer,
                                                  uint32_t header_begin,
                                                  uint32_t body_begin) const {
  byte* header = buffer + header_begin;
  // signature
  EmitUint8(&header, static_cast<uint8_t>(params_.size()));
  for (size_t i = 0; i < params_.size(); i++) {
    EmitUint8(&header, params_[i]);
  }
  EmitUint8(&header, return_type_);
  EmitUint32(&header, 0);  // name offset
  EmitUint32(&header, body_begin);
  EmitUint32(&header, body_begin + BodySize());
  EmitUint16(&header, local_int32_count_);
  EmitUint16(&header, local_int64_count_);
  EmitUint16(&header, local_float32_count_);
  EmitUint16(&header, local_float64_count_);
  EmitUint8(&header, exported_);
  EmitUint8(&header, external_);
}

void WasmFunctionEncoder::SerializeFunctionBody(byte* buffer,
                                                uint32_t body_begin) const {
  byte* body = buffer + body_begin;
  std::memcpy(body, body_.data(), body_.size());
}

WasmDataSegmentEncoder::WasmDataSegmentEncoder(Zone* z, const byte* data,
                                               uint32_t size, uint32_t dest)
    : data_(z), dest_(dest) {
  for (size_t i = 0; i < size; i++) {
    data_.push_back(data[i]);
  }
}

WasmDataSegmentEncoder::~WasmDataSegmentEncoder() {}

uint32_t WasmDataSegmentEncoder::HeaderSize() const {
  static const int kDataSegmentSize = 13;
  return kDataSegmentSize;
}

uint32_t WasmDataSegmentEncoder::BodySize() const {
  return static_cast<uint32_t>(data_.size());
}

void WasmDataSegmentEncoder::Serialize(byte* buffer, uint32_t header_begin,
                                       uint32_t body_begin) const {
  /* Header */
  byte* header = buffer + header_begin;
  EmitUint32(&header, dest_);
  EmitUint32(&header, body_begin);
  EmitUint32(&header, static_cast<uint32_t>(data_.size()));
  EmitUint8(&header, 1);  // init
  /* Body */
  byte* body = buffer + body_begin;
  std::memcpy(body, data_.data(), data_.size());
}

WasmModuleBuilder::WasmModuleBuilder(Zone* z)
    : zone_(z), functions_(z), data_segments_(z) {}

WasmModuleBuilder::~WasmModuleBuilder() {}

uint16_t WasmModuleBuilder::AddFunction() {
  functions_.push_back(
      new (zone_) WasmFunctionBuilder(zone_));
  return static_cast<uint16_t>(functions_.size() - 1);
}

WasmFunctionBuilder* WasmModuleBuilder::FunctionAt(uint16_t index) {
  if (functions_.size() > index) {
    return functions_.at(index);
  } else {
    return NULL;
  }
}

void WasmModuleBuilder::AddDataSegment(WasmDataSegmentEncoder* d) {
  data_segments_.push_back(d);
}

WasmModuleIndex::~WasmModuleIndex() {}

WasmModuleWriter* WasmModuleBuilder::Build(Zone* z) const {
  WasmModuleWriter* writer = new (z) WasmModuleWriter(z);
  for(size_t i = 0; i < functions_.size(); i++) {
    writer->functions_.push_back(functions_.at(i)->Build(z));
  }
  for(size_t i = 0; i < data_segments_.size(); i++) {
    writer->data_segments_.push_back(data_segments_.at(i));
  }
  return writer;
}

WasmModuleWriter::WasmModuleWriter(Zone* z)
    :functions_(z), data_segments_(z) {}

WasmModuleIndex* WasmModuleWriter::WriteTo(Zone* z) const {
  static const uint32_t kHeaderSize = 8;
  uint32_t total_size = kHeaderSize;
  uint32_t body_begin = kHeaderSize;
  for (size_t i = 0; i < functions_.size(); i++) {
    total_size += functions_[i]->HeaderSize();
    total_size += functions_[i]->BodySize();
    body_begin += functions_[i]->HeaderSize();
  }
  for (size_t i = 0; i < data_segments_.size(); i++) {
    total_size += data_segments_[i]->HeaderSize();
    total_size += data_segments_[i]->BodySize();
    body_begin += data_segments_[i]->HeaderSize();
  }
  ZoneVector<uint8_t> buffer_vector(total_size, z);
  byte* buffer = buffer_vector.data();
  byte* temp = buffer;
  EmitUint8(&temp, 16);
  EmitUint8(&temp, 0);
  EmitUint16(&temp, 0);  // globals
  EmitUint16(&temp, static_cast<uint16_t>(functions_.size()));
  EmitUint16(&temp, static_cast<uint16_t>(data_segments_.size()));
  uint32_t header_begin = kHeaderSize;
  for (size_t i = 0; i < functions_.size(); i++) {
    functions_[i]->Serialize(buffer, header_begin, body_begin);
    header_begin += functions_[i]->HeaderSize();
    body_begin += functions_[i]->BodySize();
  }
  for (size_t i = 0; i < data_segments_.size(); i++) {
    data_segments_[i]->Serialize(buffer, header_begin, body_begin);
    header_begin += data_segments_[i]->HeaderSize();
    body_begin += data_segments_[i]->BodySize();
  }
  return new (z) WasmModuleIndex(buffer, buffer + total_size);
}

WasmModuleWriter::~WasmModuleWriter() {}

std::vector<uint8_t> UnsignedLEB128From(uint32_t result) {
  std::vector<uint8_t> output;
  uint8_t next = 0;
  int shift = 0;
  do {
    next = static_cast<uint8_t>(result >> shift);
    if (((result >> shift) & 0xFFFFFF80) != 0) {
      next = next | 0x80;
    }
    output.push_back(next);
    shift += 7;
  } while ((next & 0x80) != 0);
  return output;
}

}
}
}
