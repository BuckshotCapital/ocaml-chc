# ocaml-chc

OCaml bindings to [clickhouse-c](https://github.com/ClickHouse/clickhouse-c),
ClickHouse's header-only C client for the Native wire format.

**Status: usable.** Queries, INSERTs and LZ4/ZSTD compression over the native
TCP protocol, plus block decoding from a file descriptor. Tested against a live
ClickHouse 26.5 and against `clickhouse local` output.

```ocaml
let c = Chc.Client.connect ~password "clickhouse.internal" in
let names, rows = Chc.Client.query_rows c "SELECT name, engine FROM system.tables LIMIT 10" in
Array.iter (fun row ->
    print_endline (String.concat " | " (Array.to_list (Array.map Chc.string_of_value row))))
  rows;
Chc.Client.close c
```

Streaming, for results that should not be materialised:

```ocaml
Chc.Client.query_iter c "SELECT number FROM numbers(10_000_000)" ~f:(fun b ->
    Array.iter consume (Chc.column b 0))
```

Writing. Column types come from the schema block the server returns for the
INSERT, so the wire types are always the server's own:

```ocaml
Chc.Client.insert c "events" [|
  [| Chc.Uint 1L; Chc.Str "hello"; Chc.Float 1.5 |];
  [| Chc.Uint 2L; Chc.Str "world"; Chc.Null   |];
|]
```

Compression is off unless asked for, and `Chc.Client.compression` reports what
was actually negotiated rather than what you requested:

```ocaml
let c = Chc.Client.connect ~compression:`Lz4 ~password host in
assert (Chc.Client.compression c = `Lz4)
```

Reading `FORMAT Native` bytes off a descriptor, with no server involved:

```ocaml
let r = Chc.open_fd (Unix.openfile "dump.native" [ Unix.O_RDONLY ] 0) in
Chc.iter r (fun b -> ...);
Chc.close r
```

## Design

**Bind the sans-IO layer, keep sockets in OCaml.** clickhouse-c ships two
client paths: `clickhouse-client.h` (owns the socket) and
`clickhouse-async.h` / `chc_in_init_ioless` (never touches one — the caller
submits bytes and drains an output buffer). We bind the latter. That means no
blocking C call ever holds the OCaml runtime lock, no threads are involved, TLS
can come from `ocaml-tls` instead of the OpenSSL header, and the same core
works under Unix, Lwt and Eio with a small per-scheduler transport shim.
`Chc.Client` is just the blocking driver over it. The separate fd reader does
block in C, and releases the runtime lock around the read.

**Hand-written C stubs, not ctypes.** The library is header-only, so a C
translation unit is required regardless — stubs are therefore free. More
importantly, `chc_column` is a tagged union and blocks are columnar: the stubs
convert a whole column per FFI crossing (`chc_stub_col_strings` materialises
every row in one pass) rather than paying per-cell overhead.

**Decode eagerly; never alias C memory.** A `chc_block` owns its entire column
tree and every `chc_column *` is interior to it. Decoded `value`s are
OCaml-owned and outlive the block. Where a raw pointer does cross into OCaml it
is a `nativeint`, and every stub taking one also takes the owning block value
so the GC keeps the target alive for the duration of the call.

**Fail loudly on header drift.** `chc_kind` ordinals are hardcoded in
`Chc.Kind`. A `_Static_assert` pins `CHC_KIND_COUNT`, so a vendored header bump
that inserts a type kind breaks the build instead of silently mis-decoding
every column after the insertion point.

## Types

Numeric, string, date/time, `Nullable`, `Array`, `Tuple`, `Map` and
`LowCardinality` (including `LowCardinality(Nullable(T))`) all decode. `Map`
surfaces as an array of pairs, matching its physical `Array(Tuple(K, V))`
layout.

Types wider than any OCaml integer render to exact text rather than being
truncated:

| ClickHouse | OCaml | Example |
|---|---|---|
| `Int128`/`Int256`/`UInt128`/`UInt256` | `Big` | `"-170141183460469231731687303715884105728"` |
| `Decimal32/64/128/256` | `Decimal` | `"1.2345"` |
| `UUID` | `Uuid` | `"61f0c404-5cb3-11e7-907b-a6006ad3dba0"` |
| `IPv4`/`IPv6` | `Ip` | `"192.168.1.1"`, `"2001:db8::1"` |

These match ClickHouse's own rendering byte for byte, which the tests assert by
comparing every decode against the server's `toString` of the same expression —
including `Int128` at its minimum, `UInt256` at its maximum, RFC 5952 `::`
compression and IPv4-mapped `IPv6`. Decimals print in shortest exact form
(trailing fractional zeros dropped, then the point), which is what both
`toString` and TSV column output do.

`Raw` remains the fallback for anything still unmodelled, holding little-endian
wire bytes rather than a guess. `JSON`, `Dynamic`, `Variant` and
`AggregateFunction` are not decodable at all: upstream does not support them in
v1.

By default `Chc.open_fd` calls `chc_column_validate` on every column, which
enforces the invariants the server itself enforces (monotonic array offsets,
in-range `LowCardinality` keys). `chc_block_read` does *not* check these, and a
forged block can otherwise drive reads past an inner column's bounds. Pass
`~validate:false` only for trusted input on a hot path.

## Build

```
direnv allow     # or: nix develop
make build
make test        # needs `clickhouse` — the flake provides it
make fmt         # ocamlformat via `dune fmt`, plus clang-format on the stubs
make fmt-check   # same, non-mutating; suitable for CI
```

`test/test_decode.ml` is hermetic — it drives `clickhouse local`, which the
flake supplies. `test/test_client.ml` needs a live server and is opt-in via
`CHC_TEST_HOST`; unset, it reports skipped and passes, so a checkout with no
server still builds green. It runs only against `numbers()` and literals, so it
works on any server and hardcodes nothing about one. Put real credentials in
`.envrc.local` (gitignored).

The flake pins OCaml 5.4 and ClickHouse 26.7 (cached for aarch64-darwin, so it
downloads rather than builds). Point `CLICKHOUSE_BIN` at another binary to test
against a different version.

The library links `lz4` and `zstd`, both supplied by the flake. They stay
dormant until a codec is requested at connect time — `clickhouse-client.h:434`
downgrades to `CHC_COMP_NONE` whenever `opts->codec` is NULL.

## Vendoring

`vendor/clickhouse-c/` holds the upstream headers at the commit recorded in
`vendor/clickhouse-c/PIN`. Upstream has no releases or tags and states the wire
format is still shifting across 25.x/26.x, so the pin is deliberate — re-vendor
explicitly and run the tests.

The directory carries its own `.clang-format` with `DisableFormat: true`:
keeping the headers byte-identical to the pinned commit is what makes a
re-vendor a reviewable diff.

## Schema-only blocks

Every TCP query response opens with a block that has column names and types,
zero rows, and — importantly — no column tree at all: `chc_block_column`
returns `NULL`. `Chc.column` returns `[||]` for it rather than dereferencing
that, and `Chc.column_layout` raises. `query_iter` passes the block through
instead of hiding it, since it is the cheapest way to learn a result's schema.

## Writing

The write path covers fixed-width leaves, `String`, the wide types above, and
`Nullable` of any of them. `Array`, `Tuple`, `Map` and `LowCardinality` decode
but do not yet encode, and raise rather than silently mis-encoding.

Wide values are parsed from text back to wire bytes in OCaml rather than in the
stub, where the parsing is memory-safe and testable; the C encoder only ever
sees fixed-width bytes. Round-trips are verified against the server's own
interpretation (`WHERE u = toUUID(...)`), not just against our own decoder,
which would pass even if encode and decode were symmetrically wrong.

Rows are transposed and shipped in batches (default 65536) rather than as one
block, flushing between batches — sends never block, so nothing else applies
backpressure.

If an INSERT fails partway, the server is left mid-statement and would reject
the next query. `insert` terminates the stream so the connection stays usable;
blocks the server already accepted stay committed, which is inherent to a
streaming insert. If even that recovery fails the connection is marked poisoned
and refuses further use with a comprehensible error instead of surfacing a
protocol violation later.

## Roadmap

1. ~~Block decoder over an fd, no TCP.~~ Done.
2. ~~Async client over OCaml sockets: handshake, query, block streaming, via
   `clickhouse-async.h`.~~ Done.
3. ~~Insert path (`chc_block_builder`) and LZ4/ZSTD compression.~~ Done.
4. ~~Wide types: 128/256-bit integers and decimals, `UUID`, `IPv4`/`IPv6` as
   first-class values, both directions.~~ Done.
5. Composite writes: `Array`, `Tuple`, `Map`, `LowCardinality`.
6. Lwt / Eio drivers — replace `Chc.Client`'s pump, reuse everything else.
