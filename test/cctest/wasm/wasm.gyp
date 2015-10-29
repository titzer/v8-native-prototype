# Copyright 2015 the V8 project authors. All rights reserved.
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

{
  'targets': [
    {
      'target_name': 'wasm_cctest',
      'type': 'none',
      # A list of additional sources and options to be injected into:
      # test/cctest/cctest.gyp:cctest
      'direct_dependent_settings': {
        'include_dirs': ['../../..'],
        'sources': [
          'test-run-wasm.cc',
          'test-run-wasm-module.cc',
        ],
      },
    },
  ],
}
