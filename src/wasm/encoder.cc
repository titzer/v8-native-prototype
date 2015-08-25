// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/signature.h"

#include "src/v8.h"
#include "src/zone-containers.h"

#include "src/wasm/encoder.h"
#include "src/wasm/wasm-module.h"
#include "src/wasm/wasm-opcodes.h"

#include "src/v8memory.h"

namespace v8 {
namespace internal {
namespace wasm {

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

WasmFunctionBuilder::WasmFunctionBuilder(Zone* z)
    : return_type_(kAstInt32),
      params_(z),
      local_int32_count_(0),
      local_int64_count_(0),
      local_float32_count_(0),
      local_float64_count_(0),
      exported_(0),
      external_(0),
      body_(z) {}

void WasmFunctionBuilder::AddParam(uint8_t param) { params_.push_back(param); }

void WasmFunctionBuilder::ReturnType(uint8_t type) { return_type_ = type; }

void WasmFunctionBuilder::AddBody(const byte* code, uint32_t size) {
  for (size_t i = 0; i < size; i++) {
    body_.push_back(code[i]);
  }
}

void WasmFunctionBuilder::Exported(uint8_t flag) { exported_ = flag; }

void WasmFunctionBuilder::External(uint8_t flag) { external_ = flag; }

void WasmFunctionBuilder::LocalInt32Count(uint16_t count) {
  local_int32_count_ = count;
}

WasmFunctionEncoder WasmFunctionBuilder::Build() const {
  return WasmFunctionEncoder::WasmFunctionEncoder(
      return_type_, params_, local_int32_count_, local_int64_count_,
      local_float32_count_, local_float64_count_, exported_, external_, body_);
}

WasmFunctionEncoder::WasmFunctionEncoder(
    uint8_t return_type, const ZoneVector<uint8_t>& params,
    uint16_t local_int32_count, uint16_t local_int64_count,
    uint16_t local_float32_count, uint16_t local_float64_count,
    uint8_t exported, uint8_t external, const ZoneVector<uint8_t>& body)
    : return_type_(return_type),
      params_(params),
      local_int32_count_(local_int32_count),
      local_int64_count_(local_int64_count),
      local_float32_count_(local_float32_count),
      local_float64_count_(local_float64_count),
      exported_(exported),
      external_(external),
      body_(body) {}

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
    : functions_(z), data_segments_(z) {}

void WasmModuleBuilder::AddFunction(const WasmFunctionEncoder& f) {
  functions_.push_back(f);
}

void WasmModuleBuilder::AddDataSegment(const WasmDataSegmentEncoder& d) {
  data_segments_.push_back(d);
}

WasmModuleIndex WasmModuleBuilder::WriteAndBuild(Zone* z) const {
  static const uint32_t kHeaderSize = 8;
  uint32_t total_size = kHeaderSize;
  uint32_t body_begin = kHeaderSize;
  for (size_t i = 0; i < functions_.size(); i++) {
    total_size += functions_[i].HeaderSize();
    total_size += functions_[i].BodySize();
    body_begin += functions_[i].HeaderSize();
  }
  for (size_t i = 0; i < data_segments_.size(); i++) {
    total_size += data_segments_[i].HeaderSize();
    total_size += data_segments_[i].BodySize();
    body_begin += data_segments_[i].HeaderSize();
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
    functions_[i].Serialize(buffer, header_begin, body_begin);
    header_begin += functions_[i].HeaderSize();
    body_begin += functions_[i].BodySize();
  }
  for (size_t i = 0; i < data_segments_.size(); i++) {
    data_segments_[i].Serialize(buffer, header_begin, body_begin);
    header_begin += data_segments_[i].HeaderSize();
    body_begin += data_segments_[i].BodySize();
  }
  return WasmModuleIndex::WasmModuleIndex(buffer, buffer + total_size);
}
}
}
}
