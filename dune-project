(lang dune 3.12)

(name chc)

; A public_name is what makes the library visible outside this project — a
; private library is scoped to the project that defines it, so a consumer
; vendoring this repo could build it but never link against it.
(package
 (name chc)
 (synopsis "OCaml bindings to clickhouse-c, ClickHouse's Native-format C client"))

; The Async driver is a separate package on purpose: `chc` itself is
; stdlib-only, and a consumer that just wants to read blocks should not have to
; build a scheduler to do it.
(package
 (name chc-async)
 (synopsis "Jane Street Async driver for chc")
 (depends chc async ppx_let))
