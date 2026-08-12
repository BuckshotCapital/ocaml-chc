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
