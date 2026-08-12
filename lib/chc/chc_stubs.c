/* chc_stubs.c — OCaml bindings for clickhouse-c, block decode path.
 *
 * This is the single translation unit that compiles the header-only library:
 * CHC_IMPLEMENTATION is defined here and nowhere else. Codecs are off — the
 * stage-1 path (FORMAT Native over an fd) never sees a compressed frame, so
 * there is no link-time dependency beyond libc.
 *
 * Memory model: a chc_block owns its whole column tree, and every chc_column*
 * is interior to it. Column and type pointers therefore cross into OCaml as
 * bare nativeints, and every stub that takes one ALSO takes the owning block
 * value. That argument is not read — it exists so the GC keeps the block (and
 * thus the pointer's target) alive across the call. Never call a column stub
 * without threading the block through.
 */

#define CHC_PROVIDE_STDLIB_ALLOC
#define CHC_IMPLEMENTATION
#define CHC_NO_LZ4
#define CHC_NO_ZSTD

#include "clickhouse.h"
#include "clickhouse-posix-io.h"

/* clickhouse-client.h pulls in clickhouse-compression.h, which is why the
 * CHC_NO_* guards above are load-bearing: without them it wants lz4.h/zstd.h.
 * The client negotiates CHC_COMP_NONE whenever opts->codec is NULL, so the
 * uncompressed TCP path needs no codec linked. */
#include "clickhouse-client.h"
#include "clickhouse-async.h"

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <stdlib.h>
#include <string.h>

/* Chc.Kind.of_int and Chc.Layout.of_int hardcode these ordinals. If a vendored
 * header bump adds a type kind, break the build here rather than silently
 * mis-decoding every column past the insertion point. */
_Static_assert(CHC_KIND_COUNT == 55, "chc_kind changed — update Chc.Kind");
_Static_assert(CHC_COL_NOTHING == 7, "chc_col_kind changed — update Chc.Layout");

/* -------------------------------------------------------------------------- */
/* Errors                                                                     */
/* -------------------------------------------------------------------------- */

static void chc_raise_error(int code, const chc_err *err) {
    CAMLparam0();
    CAMLlocal3(rec, msg, srv);

    static const value *exn = NULL;
    if (exn == NULL) {
        exn = caml_named_value("chc.error");
    }

    msg = caml_copy_string((err && err->msg[0]) ? err->msg : "unknown error");
    srv = caml_copy_string(err ? err->server_name : "");

    rec = caml_alloc(4, 0);
    Store_field(rec, 0, Val_int(code));
    Store_field(rec, 1, Val_int(err ? err->server_code : 0));
    Store_field(rec, 2, msg);
    Store_field(rec, 3, srv);

    /* Registration happens at Chc module init; if it somehow did not, degrade
     * to Failure rather than dereferencing NULL. */
    if (exn == NULL) {
        caml_failwith(String_val(msg));
    }

    caml_raise_with_arg(*exn, rec);
    CAMLnoreturn;
}

/* -------------------------------------------------------------------------- */
/* Reader handle                                                              */
/* -------------------------------------------------------------------------- */

typedef struct {
    chc_in in;
    chc_posix_io io_state;
    chc_io io;
    chc_alloc al;
    chc_block_opts opts;
    int open;
} chc_reader;

#define Reader_val(v) (*((chc_reader **) Data_custom_val(v)))

static void reader_finalize(value v) {
    chc_reader *r = Reader_val(v);
    if (r == NULL) {
        return;
    }
    if (r->open) {
        chc_in_free(&r->in);
    }
    free(r);
    Reader_val(v) = NULL;
}

static struct custom_operations reader_ops = {
    "chc.reader",
    reader_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

/* -------------------------------------------------------------------------- */
/* Block handle                                                               */
/* -------------------------------------------------------------------------- */

typedef struct {
    chc_block *b;
    chc_alloc al;
} chc_block_box;

#define Block_val(v) (*((chc_block_box **) Data_custom_val(v)))

static void block_finalize(value v) {
    chc_block_box *box = Block_val(v);
    if (box == NULL) {
        return;
    }
    if (box->b) {
        chc_block_destroy(box->b, &box->al);
    }
    free(box);
    Block_val(v) = NULL;
}

static struct custom_operations block_ops = {
    "chc.block",
    block_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

static value alloc_block(chc_block *b, const chc_alloc *al) {
    CAMLparam0();
    CAMLlocal1(v);
    chc_block_box *box = malloc(sizeof *box);
    if (box == NULL) {
        chc_block_destroy(b, al);
        caml_raise_out_of_memory();
    }
    box->b = b;
    box->al = *al;
    v = caml_alloc_custom_mem(&block_ops, sizeof(chc_block_box *), sizeof *box);
    Block_val(v) = box;
    CAMLreturn(v);
}

/* -------------------------------------------------------------------------- */
/* Reader lifecycle                                                           */
/* -------------------------------------------------------------------------- */

CAMLprim value chc_stub_reader_open(value vfd, value vblock_info, value vcustom_ser, value vbufsz) {
    CAMLparam4(vfd, vblock_info, vcustom_ser, vbufsz);
    CAMLlocal1(v);

    chc_reader *r = calloc(1, sizeof *r);
    if (r == NULL) {
        caml_raise_out_of_memory();
    }

    r->al = chc_alloc_stdlib();
    r->opts = (chc_block_opts) {
        .has_block_info = Bool_val(vblock_info),
        .has_custom_serialization = Bool_val(vcustom_ser),
        .read_buffer_bytes = (size_t) Long_val(vbufsz),
    };
    chc_posix_io_init(&r->io_state, &r->io, Int_val(vfd), NULL, NULL);

    chc_err err = {0};
    if (chc_in_init(&r->in, &r->io, &r->al, r->opts.read_buffer_bytes, &err) != CHC_OK) {
        free(r);
        chc_raise_error(CHC_ERR_IO, &err);
    }
    r->open = 1;

    v = caml_alloc_custom_mem(&reader_ops, sizeof(chc_reader *), sizeof *r);
    Reader_val(v) = r;
    CAMLreturn(v);
}

CAMLprim value chc_stub_reader_close(value vr) {
    CAMLparam1(vr);
    chc_reader *r = Reader_val(vr);
    if (r && r->open) {
        chc_in_free(&r->in);
        r->open = 0;
    }
    CAMLreturn(Val_unit);
}

/* Returns None at clean EOF, Some block otherwise. */
CAMLprim value chc_stub_read_block(value vr) {
    CAMLparam1(vr);
    CAMLlocal2(blk, some);

    chc_reader *r = Reader_val(vr);
    if (r == NULL || !r->open) {
        caml_invalid_argument("Chc.read_block: reader is closed");
    }

    chc_block *b = NULL;
    chc_err err = {0};

    /* chc_block_read blocks on the fd. Drop the runtime lock so other domains
     * and the tick thread keep running; nothing between these two calls
     * touches an OCaml value. */
    caml_release_runtime_system();
    int rc = chc_block_read(&r->in, &r->al, &r->opts, &b, &err);
    caml_acquire_runtime_system();

    if (rc != CHC_OK) {
        chc_raise_error(rc, &err);
    }
    if (b == NULL) {
        CAMLreturn(Val_int(0)); /* None */
    }

    blk = alloc_block(b, &r->al);
    some = caml_alloc(1, 0);
    Store_field(some, 0, blk);
    CAMLreturn(some);
}

/* -------------------------------------------------------------------------- */
/* Block accessors                                                            */
/* -------------------------------------------------------------------------- */

static chc_block *block_of(value v) {
    chc_block_box *box = Block_val(v);
    if (box == NULL || box->b == NULL) {
        caml_invalid_argument("Chc: block has been freed");
    }
    return box->b;
}

CAMLprim value chc_stub_block_n_rows(value vb) {
    return Val_long((long) chc_block_n_rows(block_of(vb)));
}

CAMLprim value chc_stub_block_n_columns(value vb) {
    return Val_long((long) chc_block_n_columns(block_of(vb)));
}

CAMLprim value chc_stub_block_column_name(value vb, value vi) {
    CAMLparam2(vb, vi);
    size_t len = 0;
    const char *nm = chc_block_column_name(block_of(vb), (size_t) Long_val(vi), &len);
    CAMLreturn(caml_alloc_initialized_string(len, nm ? nm : ""));
}

CAMLprim value chc_stub_block_column_ptr(value vb, value vi) {
    CAMLparam2(vb, vi);
    const chc_column *c = chc_block_column(block_of(vb), (size_t) Long_val(vi));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) c));
}

CAMLprim value chc_stub_block_type_ptr(value vb, value vi) {
    CAMLparam2(vb, vi);
    const chc_type *t = chc_block_column_type(block_of(vb), (size_t) Long_val(vi));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) t));
}

/* -------------------------------------------------------------------------- */
/* Type accessors — vb keeps the owning block alive, it is not read */
/* -------------------------------------------------------------------------- */

#define Type_ptr(v) ((const chc_type *) (uintptr_t) Nativeint_val(v))
#define Col_ptr(v) ((const chc_column *) (uintptr_t) Nativeint_val(v))

CAMLprim value chc_stub_type_kind(value vb, value vt) {
    (void) vb;
    return Val_long((long) chc_type_kind(Type_ptr(vt)));
}

CAMLprim value chc_stub_type_n_children(value vb, value vt) {
    (void) vb;
    return Val_long((long) chc_type_n_children(Type_ptr(vt)));
}

CAMLprim value chc_stub_type_child(value vb, value vt, value vi) {
    CAMLparam3(vb, vt, vi);
    const chc_type *c = chc_type_child(Type_ptr(vt), (size_t) Long_val(vi));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) c));
}

CAMLprim value chc_stub_type_format(value vb, value vt) {
    CAMLparam2(vb, vt);
    CAMLlocal1(s);
    const chc_type *t = Type_ptr(vt);

    /* snprintf-style: first call sizes the buffer. */
    size_t need = chc_type_format(t, NULL, 0);
    char *buf = malloc(need + 1);
    if (buf == NULL) {
        caml_raise_out_of_memory();
    }
    (void) chc_type_format(t, buf, need + 1);
    s = caml_alloc_initialized_string(need, buf);
    free(buf);
    CAMLreturn(s);
}

CAMLprim value chc_stub_type_datetime64_scale(value vb, value vt) {
    (void) vb;
    return Val_long((long) chc_type_datetime64_scale(Type_ptr(vt)));
}

CAMLprim value chc_stub_type_decimal_scale(value vb, value vt) {
    (void) vb;
    return Val_long((long) chc_type_decimal_scale(Type_ptr(vt)));
}

/* -------------------------------------------------------------------------- */
/* Column accessors                                                           */
/* -------------------------------------------------------------------------- */

CAMLprim value chc_stub_col_layout(value vb, value vc) {
    (void) vb;
    return Val_long((long) chc_column_layout(Col_ptr(vc)));
}

CAMLprim value chc_stub_col_n_rows(value vb, value vc) {
    (void) vb;
    return Val_long((long) chc_column_n_rows(Col_ptr(vc)));
}

/* Untrusted input: walk the tree and enforce the invariants the server itself
 * enforces (monotonic array offsets, in-range LC keys). chc_block_read does
 * not do this, and a forged block can otherwise walk us off the end of an
 * inner column. */
CAMLprim value chc_stub_col_validate(value vb, value vc) {
    CAMLparam2(vb, vc);
    chc_err err = {0};
    if (chc_column_validate(Col_ptr(vc), &err) != CHC_OK) {
        chc_raise_error(CHC_ERR_PROTOCOL, &err);
    }
    CAMLreturn(Val_unit);
}

/* FIXED: returns (raw_bytes, elem_size). Little-endian on the wire; the OCaml
 * side decodes with String.get_intNN_le, so this is correct on BE hosts too. */
CAMLprim value chc_stub_col_fixed(value vb, value vc) {
    CAMLparam2(vb, vc);
    CAMLlocal2(s, pair);
    const chc_column *c = Col_ptr(vc);

    size_t elem = 0;
    const void *d = chc_column_fixed_data(c, &elem);
    size_t n = chc_column_n_rows(c);

    s = caml_alloc_initialized_string(n * elem, d ? (const char *) d : "");
    pair = caml_alloc(2, 0);
    Store_field(pair, 0, s);
    Store_field(pair, 1, Val_long((long) elem));
    CAMLreturn(pair);
}

/* STRING: bulk-materialize every row in one pass. Doing this per-cell across
 * the FFI boundary is what makes naive bindings slow. */
CAMLprim value chc_stub_col_strings(value vb, value vc) {
    CAMLparam2(vb, vc);
    CAMLlocal2(arr, s);
    const chc_column *c = Col_ptr(vc);

    size_t n = chc_column_n_rows(c);
    const uint8_t *data = chc_column_string_data(c);
    const uint64_t *offs = chc_column_string_offsets(c);

    if (n == 0) {
        CAMLreturn(Atom(0));
    }
    if (data == NULL || offs == NULL) {
        caml_invalid_argument("Chc: malformed string column");
    }

    arr = caml_alloc(n, 0);
    uint64_t prev = 0;
    for (size_t i = 0; i < n; i++) {
        uint64_t end = offs[i];
        if (end < prev) {
            caml_invalid_argument("Chc: string offsets go backwards");
        }
        s = caml_alloc_initialized_string((size_t) (end - prev), (const char *) data + prev);
        Store_field(arr, i, s);
        prev = end;
    }
    CAMLreturn(arr);
}

CAMLprim value chc_stub_col_null_map(value vb, value vc) {
    CAMLparam2(vb, vc);
    const chc_column *c = Col_ptr(vc);
    size_t n = chc_column_n_rows(c);
    const uint8_t *m = chc_column_null_map(c);
    CAMLreturn(caml_alloc_initialized_string(n, m ? (const char *) m : ""));
}

CAMLprim value chc_stub_col_nullable_inner(value vb, value vc) {
    CAMLparam2(vb, vc);
    const chc_column *i = chc_column_nullable_inner(Col_ptr(vc));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) i));
}

/* Cumulative exclusive ends; fits an OCaml int on 64-bit hosts. */
CAMLprim value chc_stub_col_array_offsets(value vb, value vc) {
    CAMLparam2(vb, vc);
    CAMLlocal1(arr);
    const chc_column *c = Col_ptr(vc);
    size_t n = chc_column_n_rows(c);
    const uint64_t *offs = chc_column_array_offsets(c);

    if (n == 0) {
        CAMLreturn(Atom(0));
    }
    if (offs == NULL) {
        caml_invalid_argument("Chc: malformed array column");
    }

    arr = caml_alloc(n, 0);
    for (size_t i = 0; i < n; i++) {
        Store_field(arr, i, Val_long((long) offs[i]));
    }
    CAMLreturn(arr);
}

CAMLprim value chc_stub_col_array_values(value vb, value vc) {
    CAMLparam2(vb, vc);
    const chc_column *v = chc_column_array_values(Col_ptr(vc));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) v));
}

CAMLprim value chc_stub_col_tuple_arity(value vb, value vc) {
    (void) vb;
    return Val_long((long) chc_column_tuple_arity(Col_ptr(vc)));
}

CAMLprim value chc_stub_col_tuple_child(value vb, value vc, value vi) {
    CAMLparam3(vb, vc, vi);
    const chc_column *ch = chc_column_tuple_child(Col_ptr(vc), (size_t) Long_val(vi));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) ch));
}

CAMLprim value chc_stub_col_lc_key_size(value vb, value vc) {
    (void) vb;
    return Val_long((long) chc_column_lc_key_size(Col_ptr(vc)));
}

/* Dictionary indices, widened to OCaml ints. Host byte order already. */
CAMLprim value chc_stub_col_lc_keys(value vb, value vc) {
    CAMLparam2(vb, vc);
    CAMLlocal1(arr);
    const chc_column *c = Col_ptr(vc);
    size_t n = chc_column_n_rows(c);
    int ks = chc_column_lc_key_size(c);
    const void *keys = chc_column_lc_keys(c);

    if (n == 0) {
        CAMLreturn(Atom(0));
    }
    if (keys == NULL) {
        caml_invalid_argument("Chc: malformed LowCardinality column");
    }

    arr = caml_alloc(n, 0);
    for (size_t i = 0; i < n; i++) {
        uint64_t k;
        switch (ks) {
        case 1:
            k = ((const uint8_t *) keys)[i];
            break;
        case 2:
            k = ((const uint16_t *) keys)[i];
            break;
        case 4:
            k = ((const uint32_t *) keys)[i];
            break;
        case 8:
            k = ((const uint64_t *) keys)[i];
            break;
        default:
            caml_invalid_argument("Chc: bad LowCardinality key size");
        }
        Store_field(arr, i, Val_long((long) k));
    }
    CAMLreturn(arr);
}

CAMLprim value chc_stub_col_lc_dict(value vb, value vc) {
    CAMLparam2(vb, vc);
    const chc_column *d = chc_column_lc_dict(Col_ptr(vc));
    CAMLreturn(caml_copy_nativeint((intnat) (uintptr_t) d));
}

/* -------------------------------------------------------------------------- */
/* Async client                                                               */
/* -------------------------------------------------------------------------- */

/* These tags MUST match the declaration order of Chc.Async.packet in chc.ml.
 * OCaml numbers constant and non-constant constructors in two independent
 * sequences, each in source order, so the two blocks below are separate. */
enum {
    PKT_CONST_PONG = 0,
    PKT_CONST_END_OF_STREAM = 1,
    PKT_CONST_TABLE_COLUMNS = 2,
    PKT_CONST_HELLO = 3,
};
enum {
    PKT_TAG_DATA = 0,
    PKT_TAG_TOTALS = 1,
    PKT_TAG_EXTREMES = 2,
    PKT_TAG_LOG = 3,
    PKT_TAG_PROFILE_EVENTS = 4,
    PKT_TAG_EXCEPTION = 5,
    PKT_TAG_PROGRESS = 6,
    PKT_TAG_PROFILE_INFO = 7,
};

typedef struct {
    chc_async_client *c;
    /* The client keeps this allocator BY POINTER (clickhouse-async.h:124,
     * `c->cli.al = al`) rather than copying it, so it has to outlive the
     * client. Passing a stack chc_alloc to init would be a use-after-free. */
    chc_alloc al;
    int open;
} chc_async_box;

#define Async_val(v) (*((chc_async_box **) Data_custom_val(v)))

static void async_finalize(value v) {
    chc_async_box *box = Async_val(v);
    if (box == NULL) {
        return;
    }
    if (box->open && box->c) {
        chc_async_client_free(box->c);
    }
    free(box);
    Async_val(v) = NULL;
}

static struct custom_operations async_ops = {
    "chc.async",
    async_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

static chc_async_box *async_of(value v) {
    chc_async_box *box = Async_val(v);
    if (box == NULL || !box->open) {
        caml_invalid_argument("Chc.Async: client is closed");
    }
    return box;
}

CAMLprim value chc_stub_async_create(value vname, value vdb, value vuser, value vpass, value vbufsz) {
    CAMLparam5(vname, vdb, vuser, vpass, vbufsz);
    CAMLlocal1(v);

    chc_async_box *box = calloc(1, sizeof *box);
    if (box == NULL) {
        caml_raise_out_of_memory();
    }
    box->al = chc_alloc_stdlib();

    /* Hello strings are copied internally, so these borrows only need to be
     * valid for the duration of the call. No OCaml allocation happens inside
     * init (it allocates through chc_alloc, i.e. malloc), so the GC cannot
     * move them underneath it. */
    chc_client_opts opts = {0};
    opts.client_name = String_val(vname);
    opts.database = String_val(vdb);
    opts.user = String_val(vuser);
    opts.password = String_val(vpass);
    opts.read_buffer_bytes = (size_t) Long_val(vbufsz);

    chc_err err = {0};
    if (chc_async_client_init(&box->c, &opts, &box->al, &err) != CHC_OK) {
        free(box);
        chc_raise_error(CHC_ERR_USAGE, &err);
    }
    box->open = 1;

    v = caml_alloc_custom_mem(&async_ops, sizeof(chc_async_box *), sizeof *box);
    Async_val(v) = box;
    CAMLreturn(v);
}

CAMLprim value chc_stub_async_close(value vh) {
    CAMLparam1(vh);
    chc_async_box *box = Async_val(vh);
    if (box && box->open) {
        chc_async_client_free(box->c);
        box->c = NULL;
        box->open = 0;
    }
    CAMLreturn(Val_unit);
}

/* true = handshake complete, false = needs more inbound bytes. */
CAMLprim value chc_stub_async_handshake(value vh) {
    CAMLparam1(vh);
    chc_async_box *box = async_of(vh);
    chc_err err = {0};
    int rc = chc_async_handshake(box->c, &err);
    if (rc == CHC_OK) {
        CAMLreturn(Val_true);
    }
    if (rc == CHC_WOULD_BLOCK) {
        CAMLreturn(Val_false);
    }
    chc_raise_error(rc, &err);
    CAMLreturn(Val_false); /* unreachable */
}

CAMLprim value chc_stub_async_send_query(value vh, value vsql, value vqid) {
    CAMLparam3(vh, vsql, vqid);
    chc_async_box *box = async_of(vh);
    chc_err err = {0};
    if (chc_async_send_query(box->c, String_val(vsql), caml_string_length(vsql), String_val(vqid), caml_string_length(vqid), &err) !=
        CHC_OK) {
        chc_raise_error(CHC_ERR_PROTOCOL, &err);
    }
    CAMLreturn(Val_unit);
}

/* Terminates a query's data stream (and an INSERT's rows) with an empty block. */
CAMLprim value chc_stub_async_send_data_end(value vh) {
    CAMLparam1(vh);
    chc_async_box *box = async_of(vh);
    chc_err err = {0};
    if (chc_async_send_data_end(box->c, &err) != CHC_OK) {
        chc_raise_error(CHC_ERR_PROTOCOL, &err);
    }
    CAMLreturn(Val_unit);
}

/* Inbound socket bytes. chc_in_submit copies into the client's own buffer, and
 * allocates through chc_alloc rather than the OCaml heap, so handing it a
 * pointer into vbuf is safe for the duration of the call. */
CAMLprim value chc_stub_async_submit(value vh, value vbuf, value vlen) {
    CAMLparam3(vh, vbuf, vlen);
    chc_async_box *box = async_of(vh);
    chc_err err = {0};
    if (chc_async_submit(box->c, Bytes_val(vbuf), (size_t) Long_val(vlen), &err) != CHC_OK) {
        chc_raise_error(CHC_ERR_IO, &err);
    }
    CAMLreturn(Val_unit);
}

CAMLprim value chc_stub_async_pending_out(value vh) {
    CAMLparam1(vh);
    chc_async_box *box = async_of(vh);
    const uint8_t *buf = NULL;
    size_t len = 0;
    chc_async_pending_out(box->c, &buf, &len);
    CAMLreturn(caml_alloc_initialized_string(len, buf ? (const char *) buf : ""));
}

CAMLprim value chc_stub_async_consume_out(value vh, value vn) {
    CAMLparam2(vh, vn);
    chc_async_box *box = async_of(vh);
    chc_async_consume_out(box->c, (size_t) Long_val(vn));
    CAMLreturn(Val_unit);
}

CAMLprim value chc_stub_async_server_info(value vh) {
    CAMLparam1(vh);
    CAMLlocal4(rec, s_name, s_tz, s_disp);
    chc_async_box *box = async_of(vh);
    const chc_server_info *si = chc_async_server_info(box->c);

    s_name = caml_copy_string(si ? si->name : "");
    s_tz = caml_copy_string(si ? si->timezone : "");
    s_disp = caml_copy_string(si ? si->display_name : "");

    rec = caml_alloc(7, 0);
    Store_field(rec, 0, s_name);
    Store_field(rec, 1, s_tz);
    Store_field(rec, 2, s_disp);
    Store_field(rec, 3, Val_long(si ? (long) si->version_major : 0));
    Store_field(rec, 4, Val_long(si ? (long) si->version_minor : 0));
    Store_field(rec, 5, Val_long(si ? (long) si->version_patch : 0));
    Store_field(rec, 6, Val_long(si ? (long) si->revision : 0));
    CAMLreturn(rec);
}

/* Returns None when the input buffer drained mid-parse (submit more bytes and
 * retry — parse state is preserved), Some packet otherwise. Server-side errors
 * arrive as a CHC_PKT_EXCEPTION packet with CHC_OK, not as a hard failure. */
CAMLprim value chc_stub_async_recv_packet(value vh) {
    CAMLparam1(vh);
    CAMLlocal5(res, pay, inner, s1, s2);
    CAMLlocal1(s3);

    chc_async_box *box = async_of(vh);
    chc_packet pkt = {0};
    chc_err err = {0};

    int rc = chc_async_recv_packet(box->c, &pkt, &err);
    if (rc == CHC_WOULD_BLOCK) {
        CAMLreturn(Val_int(0)); /* None */
    }
    if (rc != CHC_OK) {
        chc_raise_error(rc, &err);
    }

    int block_tag = -1;
    int const_tag = -1;
    switch (pkt.kind) {
    case CHC_PKT_DATA:
        block_tag = PKT_TAG_DATA;
        break;
    case CHC_PKT_TOTALS:
        block_tag = PKT_TAG_TOTALS;
        break;
    case CHC_PKT_EXTREMES:
        block_tag = PKT_TAG_EXTREMES;
        break;
    case CHC_PKT_LOG:
        block_tag = PKT_TAG_LOG;
        break;
    case CHC_PKT_PROFILE_EVENTS:
        block_tag = PKT_TAG_PROFILE_EVENTS;
        break;
    case CHC_PKT_PONG:
        const_tag = PKT_CONST_PONG;
        break;
    case CHC_PKT_END_OF_STREAM:
        const_tag = PKT_CONST_END_OF_STREAM;
        break;
    case CHC_PKT_TABLE_COLUMNS:
        const_tag = PKT_CONST_TABLE_COLUMNS;
        break;
    case CHC_PKT_HELLO:
        const_tag = PKT_CONST_HELLO;
        break;
    default:
        break;
    }

    if (block_tag >= 0) {
        chc_block *b = pkt.block;
        pkt.block = NULL; /* take ownership before clearing */
        chc_async_packet_clear(box->c, &pkt);
        inner = alloc_block(b, &box->al);
        pay = caml_alloc(1, block_tag);
        Store_field(pay, 0, inner);
    } else if (const_tag >= 0) {
        chc_async_packet_clear(box->c, &pkt);
        pay = Val_int(const_tag);
    } else if (pkt.kind == CHC_PKT_EXCEPTION) {
        const chc_exception *e = pkt.exception;
        /* Materialise before clearing — packet_clear frees the exception. */
        s1 = caml_alloc_initialized_string(e && e->name ? e->name_len : 0, (e && e->name) ? e->name : "");
        s2 = caml_alloc_initialized_string(e && e->display_text ? e->display_text_len : 0, (e && e->display_text) ? e->display_text : "");
        s3 = caml_alloc_initialized_string(e && e->stack_trace ? e->stack_trace_len : 0, (e && e->stack_trace) ? e->stack_trace : "");
        inner = caml_alloc(4, 0);
        Store_field(inner, 0, Val_long(e ? (long) e->code : 0));
        Store_field(inner, 1, s1);
        Store_field(inner, 2, s2);
        Store_field(inner, 3, s3);
        chc_async_packet_clear(box->c, &pkt);
        pay = caml_alloc(1, PKT_TAG_EXCEPTION);
        Store_field(pay, 0, inner);
    } else if (pkt.kind == CHC_PKT_PROGRESS) {
        inner = caml_alloc(5, 0);
        Store_field(inner, 0, Val_long((long) pkt.progress.rows));
        Store_field(inner, 1, Val_long((long) pkt.progress.bytes));
        Store_field(inner, 2, Val_long((long) pkt.progress.total_rows));
        Store_field(inner, 3, Val_long((long) pkt.progress.written_rows));
        Store_field(inner, 4, Val_long((long) pkt.progress.written_bytes));
        chc_async_packet_clear(box->c, &pkt);
        pay = caml_alloc(1, PKT_TAG_PROGRESS);
        Store_field(pay, 0, inner);
    } else if (pkt.kind == CHC_PKT_PROFILE_INFO) {
        inner = caml_alloc(6, 0);
        Store_field(inner, 0, Val_long((long) pkt.profile.rows));
        Store_field(inner, 1, Val_long((long) pkt.profile.blocks));
        Store_field(inner, 2, Val_long((long) pkt.profile.bytes));
        Store_field(inner, 3, Val_long((long) pkt.profile.rows_before_limit));
        Store_field(inner, 4, Val_bool(pkt.profile.applied_limit));
        Store_field(inner, 5, Val_bool(pkt.profile.calculated_rows_before_limit));
        chc_async_packet_clear(box->c, &pkt);
        pay = caml_alloc(1, PKT_TAG_PROFILE_INFO);
        Store_field(pay, 0, inner);
    } else {
        chc_async_packet_clear(box->c, &pkt);
        caml_invalid_argument("Chc.Async: unknown packet kind from server");
    }

    res = caml_alloc(1, 0); /* Some */
    Store_field(res, 0, pay);
    CAMLreturn(res);
}
