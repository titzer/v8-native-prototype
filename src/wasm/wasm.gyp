# Copyright 2015 the V8 project authors. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

{
  'targets': [
    {
      'target_name': 'wasm',
      'toolsets': ['target', 'host'],
      'type': 'none',
      # A list of additional sources and options to be injected into:
      # tools/gyp/v8.gyp:v8_base
      'direct_dependent_settings': {
        'include_dirs': ['../..'],
        'sources': [
          'asm-wasm-builder.cc',
          'asm-wasm-builder.h',
          'ast-decoder.cc',
          'ast-decoder.h',
          'encoder.cc',
          'encoder.h',
	  'module-decoder.cc',
	  'module-decoder.h',
          'tf-builder.h',
          'tf-builder.cc',
          'wasm-js.cc',
          'wasm-js.h',
          'wasm-linkage.cc',
          'wasm-macro-gen.h',
          'wasm-module.cc',
          'wasm-module.h',
          'wasm-opcodes.cc',
          'wasm-opcodes.h',
          'wasm-result.cc',
          'wasm-result.h',
          'wasm-wrapper.cc',
          'wasm-wrapper.h',
        ],
      },
    },
  ],
}
