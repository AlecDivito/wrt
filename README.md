# WRT

Web assembly runtime.

## Why

idk, kinda have wanted to do this for awhile and i finally have time. :) I'll
update this readme when this is closer to a beta release!

```wasm
;; Test t.const instructions

    ;; Syntax error

    (; test ;)
    
    (module (func (i32.const 0_123_456_789) drop))
    (module (func (i32.const 0x0_9acf_fBDF) drop))
    (assert_malformed
        (module quote "(func (i32.const) drop)")
        "unexpected token"
    )
```

## Download Test Suite

Because Web assembly is an existing standard, we can use the existing test cases
to validate that our implementation is working correctly. We can download all
the tests from [Webassembly/spec](https://github.com/WebAssembly/spec/tree/main/test/core).
If you are contributing to the repository, please run the command below to download
all the tests files locally.

```bash
mkdir testsuite
cd testsuite
git clone https://github.com/WebAssembly/spec.git
find . -name "*wast" -type f -exec mv {} temp \;
rm -rf spec
mv temp/* .
rmdir temp
```
