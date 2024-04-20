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