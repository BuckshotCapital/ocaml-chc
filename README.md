# ocaml-chc

OCaml bindings to [clickhouse-c](https://github.com/ClickHouse/clickhouse-c),
ClickHouse's header-only C client for the Native wire format.

**Status: usable.** Queries, INSERTs and LZ4/ZSTD compression over the native
TCP protocol, plus block decoding from a file descriptor. A Jane Street Async
driver with a streaming inserter ships alongside as `chc-async`. Tested against
live ClickHouse 26.5 and 26.7, and against `clickhouse local` output.

```ocaml
let c = Chc.Client.connect ~password "clickhouse.internal" in
let names, rows = Chc.Client.query_rows c "SELECT name, engine FROM system.tables LIMIT 10" in
Array.iter (fun row ->
    print_endline (String.concat " | " (Array.to_list (Array.map Chc.string_of_value row))))
  rows;
Chc.Client.close c
```

Rows decode into records by column name, so a change in `SELECT` order cannot
silently reinterpret them:

```ocaml
type spread = { asset : string; adj : float; markets : int }

let spread =
  let open Chc.Row in
  let+ asset   = field "asset" string
  and+ adj     = field "adj" float
  and+ markets = field "markets" int in
  { asset; adj; markets }

let top = Chc.Client.fetch c "SELECT asset, adj, markets FROM ..." spread
```

A missing column or a type mismatch raises `Chc.Row.Decode_error` naming both
sides — `column "a" (String) row 0: expected an integer, got text`, or
`no column "nope"; block has [a]`. Each column is decoded at most once per
block and only if a field names it, so selecting ten and reading two costs two
decodes.

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
works under any scheduler with a small per-scheduler transport shim.
`Chc.Client` is just the blocking driver over it and `chc-async` the Jane
Street Async one; each replaces the pump and nothing else. The separate fd
reader does block in C, and releases the runtime lock around the read.

The module is called `Chc.Protocol` rather than `Chc.Async` for the obvious
reason: a program driving it with Jane Street's Async has that name in scope
already.

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
`LowCardinality` (including `LowCardinality(Nullable(T))`) all work in both
directions, nested to any depth. `Map` surfaces as an array of pairs, matching
its physical `Array(Tuple(K, V))` layout.

Types wider than any OCaml integer render to exact text rather than being
truncated:

| ClickHouse | OCaml | Example |
|---|---|---|
| `Int128`/`Int256`/`UInt128`/`UInt256` | `Big` | `"-170141183460469231731687303715884105728"` |
| `Decimal32/64/128/256` | `Decimal` | `"1.2345"` |
| `UUID` | `Uuid` | `"61f0c404-5cb3-11e7-907b-a6006ad3dba0"` |
| `IPv4`/`IPv6` | `Ip` | `"192.168.1.1"`, `"2001:db8::1"` |

These match ClickHouse's own rendering byte for byte, which the tests assert by
comparing every decode against the server's `toString` of the same expression,
under both IP backends (see [Optional dependencies](#optional-dependencies)) —
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

## Optional dependencies

OCaml has no equivalent of Cargo features — no consumer-side `--features`, and
no way for a downstream package to request one. What dune offers instead is
`(select)`, which picks an implementation by whether a library is *available*:

```
(libraries
 unix
 (select chc_ip.ml from
  (ipaddr -> chc_ip.ipaddr.ml)
  (-> chc_ip.fallback.ml)))
```

IPv4/IPv6 conversion goes through that. With
[`ipaddr`](https://github.com/mirage/ocaml-ipaddr) installed you get its
implementation; without it, the hand-rolled fallback. `Chc.ip_backend` reports
which one was compiled in.

The constraint that makes this safe is `chc_ip.mli`: both backends satisfy one
signature that deals only in `string`, so the choice **cannot reach
`Chc.value`**. A `(select)` that changed a public type would be worse than no
abstraction at all — the API would differ by what happened to be installed,
invisibly, and downstream code would break in ways the signature never hinted
at. That is the same hazard behind Rust's "features must be additive" rule,
with sharper teeth here because it surfaces as a type error.

For the same reason the wide-integer and decimal rendering is *not* optional:
it is used in core decoding, so making it `zarith`-backed would turn a GMP
dependency into a hard requirement for every user. Those 83 lines of long
division stay, verified against the server at `Int128` min/max and `UInt256`
max.

## Build

```
direnv allow     # or: nix develop
just             # lists the recipes
just build
just test        # needs `clickhouse` — the flake provides it
just test-live   # the same, against a server it starts and tears down
just fmt         # ocamlformat via `dune fmt`, plus clang-format on the stubs
just fmt-check   # same, non-mutating; suitable for CI
```

`test/test_decode.ml` is hermetic — it drives `clickhouse local`, which the
flake supplies. `test/test_client.ml` and `test/test_async.ml` need a live
server and are opt-in via `CHC_TEST_HOST`; unset, they report skipped and pass,
so a checkout with no server still builds green. They run against `numbers()`,
literals, and tables they create and drop themselves, so they work on any
server and hardcode nothing about one. Put real credentials in `.envrc.local`
(gitignored).

The live tests create and drop tables, so point them at something disposable
rather than at an instance you care about. `just test-live` is that path: it
starts a ClickHouse from the flake in a temp directory, overrides every
`CHC_TEST_*` variable `.envrc.local` may have set, runs the suite and tears the
server down again.

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

The write path covers fixed-width leaves, `String`, the wide types above,
`Nullable` of any of them, and `Array`, `Tuple`, `Map` and `LowCardinality`
nested to any depth — `Map(String, Array(Float64))` and
`Array(LowCardinality(Nullable(String)))` both round-trip. Values take the same
shapes decoding produces, so a row read back can be written straight out again:

```ocaml
Chc.Client.insert c "events" [|
  [| Chc.Arr [| Chc.Str "a"; Chc.Str "b" |]                          (* Array(String) *)
   ; Chc.Tup [| Chc.Uint 7L; Chc.Str "seven" |]                      (* Tuple(UInt8, String) *)
   ; Chc.Arr [| Chc.Tup [| Chc.Str "k"; Chc.Uint 1L |] |]            (* Map(String, UInt32) *)
  |];
|]
```

A value whose shape does not match its column raises `Invalid_argument` naming
the column, the row and both sides — `column "tags" row 3: Array(String)
expects an array, got text` — before any of that block reaches the socket.
`JSON`, `Dynamic`, `Variant`, the geo types and `AggregateFunction` state stay
unwritable, and are refused rather than mis-encoded.

Composites are flattened in OCaml, not in C: an `Array` becomes cumulative
offsets plus its elements one level down, a `Tuple` transposes into one column
per field, and a `Map` is an `Array` over a two-field `Tuple` — the physical
layout the reader already exposes, run backwards. Each wants a growable buffer,
a transpose or a hash table, so doing it on the OCaml side keeps the stub a
layout copy. Offsets are per block, so a row past the 65536-row batch boundary
is the case worth testing, and is.

`LowCardinality` is the one column type that is not a straight serialisation —
it needs a dictionary of distinct values, a per-row index into it, and a key
width chosen to fit. The dictionary is built in OCaml, where a hash table is
free; C gets a finished dictionary and index array, so the stub stays a layout
copy like every other path. Slot 0 is reserved for the type default (`NULL`
when the inner type is `Nullable`, otherwise the empty string), which is the
convention the reader and the server both assume.

This matters more than it sounds: `LowCardinality(String)` is how real
ClickHouse schemas spell every enum-ish column, so without it a driver can read
a production table but not write one.

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
5. ~~Composite writes: `Array`, `Tuple`, `Map`, `LowCardinality`.~~ Done.
6. ~~Jane Street Async driver, with a streaming inserter.~~ Done — `chc-async`.
7. Lwt / Eio drivers — the same shim again, if anyone wants them.

## Async driver

`chc-async` is a separate package: `chc` itself is stdlib-only, and a consumer
that just wants to read blocks should not have to build a scheduler to do it.

Results arrive as a pipe, so the consumer drives the read — the connection
stops pulling from the socket while the pipe is full, rather than accumulating
blocks in memory:

```ocaml
let%bind conn = Chc_async.connect ~password host in
Pipe.iter (Chc_async.query_pipe conn "SELECT ...") ~f:handle_block
```

A connection runs one statement at a time; overlapping calls queue rather than
interleave. Abandoning a pipe early is fine — the rest of the response is
drained in the background and the connection stays usable.

### Streaming inserts

Finishing an INSERT statement is the expensive part. Measured against a local
26.7, one `INSERT ... VALUES` costs **~60 ms** however few rows it carries —
and that is the server's, not the client's: `clickhouse client` pays 61 ms for
the same statement, against `Null`, `Memory` and `MergeTree` alike, while a
bare round trip on the same connection is 0.2 ms and `INSERT ... SELECT` is
0.5 ms.

Pushing another block into a statement that is *already open* costs 0.055 ms.
So a writer fed by a stream should open once and flush often, which is what
`Inserter` is:

```ocaml
let%bind ins = Chc_async.Inserter.create conn "funding_rates" ~max_rows:50_000 in
Pipe.iter rows ~f:(fun row ->
  let%bind () = Chc_async.Inserter.write ins (encode row) in
  Chc_async.Inserter.commit ins)
```

`commit` flushes only when a threshold is crossed, so it is cheap to call on
every turn of a select loop, and a periodic tick that calls it is what turns
time into a flush. The timer stays in the scheduler rather than in here.

The test asserts the property that matters: 20 000 rows over 40 flushes land as
**one part**, which only happens if the statement stayed open the whole time.
Closing and reopening per flush costs the same as not batching at all.

An inserter takes its connection over for its lifetime — queries on it raise
until `close` — so a writer wants its own. There is no rollback: blocks the
server has accepted stay committed if a later one fails.

## Query parameters

Values bind server-side via `{name:Type}` placeholders rather than being pasted
into SQL:

```ocaml
Chc.Client.fetch c
  "SELECT * FROM t WHERE venue NOT IN {skip:Array(String)} AND oi >= {floor:Float64}"
  ~params:[ "skip", Chc.Param.(array (List.map string [ "dydx"; "arcus" ]))
          ; "floor", Chc.Param.float 100_000. ]
  row
```

Two things about this are worth knowing, both measured against 26.5 rather than
taken from the docs:

- **Every value crosses as a quoted string literal**, whatever the placeholder
  declares. `{v:Int64}` rejects a bare `42` with *"Couldn't restore Field from
  dump"* and accepts `'42'`. `clickhouse-client.h` documents the opposite.
- **The server unescapes the carried literal twice** — once parsing the Field,
  once parsing the value's own text form. A string containing a backslash comes
  back corrupted if escaped only once. `Chc.Param` escapes everything to the
  same depth, so a value containing quotes, backslashes and `; DROP TABLE`
  round-trips byte for byte.

Because clickhouse-c passes param values through verbatim, going via
`Chc.Param` is what makes a parameterised query actually safe rather than
differently-shaped interpolation. Note also that a *malformed* parameter closes
the connection, unlike an ordinary SQL error.

## Caveat: AggregateFunction columns

`clickhouse-c` v1 cannot decode `AggregateFunction` state, so a column like
`AggregateFunction(argMax, Float64, DateTime64)` must be merged in SQL —
`argMaxMerge(state)` returns a plain `Float64` and decodes fine. Selecting the
raw state will fail. Same for `JSON`, `Dynamic` and `Variant`.
