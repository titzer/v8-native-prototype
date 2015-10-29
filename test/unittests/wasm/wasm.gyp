# Copyright 2015 the V8 project authors. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

{
  'targets': [
    {
      'target_name': 'wasm_unittests',
      'type': 'none',
      # A list of additional sources and options to be injected into:
      # test/unittests/unittests.gyp:unittests
      'direct_dependent_settings': {
        'include_dirs': ['../../..'],
        'sources': [
          'ast-decoder-unittest.cc',
          'wasm-macro-gen-unittest.cc',
          'module-decoder-unittest.cc',
	  'encoder-unittest.cc',
        ],
      },
    },
  ],
}
