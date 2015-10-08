// Copyright 2015 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

function foo() {
  "use asm";
  function sum(a, b) {
    a = a|0;
    c = b|0 + 1;
    return a + c + 1;
  }

  function caller() {
    return sum(77, 22);
  }

  return {caller: caller};
}

assertEquals(101, WASM.asmCompileRun(foo.toString()));
