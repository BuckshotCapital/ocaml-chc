(lang dune 3.12)

(name chc)

; A public_name is what makes the library visible outside this project — a
; private library is scoped to the project that defines it, so a consumer
; vendoring this repo could build it but never link against it.
(package
 (name chc)
 (synopsis "OCaml bindings to clickhouse-c, ClickHouse's Native-format C client"))
