# ocaml-chc task runner. `just` on its own lists what is here.
#
# Only the last comment line above a recipe becomes its `--list` description,
# so the explanations sit above a blank line and the one-liners sit flush.

# Port for the throwaway server `test-live` starts. Deliberately not 9000, so it
# cannot collide with a real instance: `just test-live port=19001`.
port := "19000"

default:
    @just --list

# Build everything, library and tests.
build:
    dune build @all

# --force because dune does not track CHC_TEST_HOST as a dependency: without it
# a `just test` straight after a `just test-live` reports nothing at all rather
# than re-running against no server. A full forced run is a couple of seconds.

# Run the tests; the live ones skip unless CHC_TEST_HOST is set.
test:
    dune test --force

# dune fmt covers .ml/.mli and dune files; clang-format covers the stubs.
# vendor/ carries a DisableFormat .clang-format, so it is left untouched.

# Format in place.
fmt:
    -dune fmt
    clang-format -i lib/chc/*.c

# Check formatting without touching anything; suitable for CI.
fmt-check:
    dune build @fmt
    clang-format --dry-run --Werror lib/chc/*.c

# Remove build artefacts.
clean:
    dune clean

# The live tests create and drop tables, so this exists to keep the easy path
# off any instance you care about: .envrc.local points CHC_TEST_HOST at a real
# server, and every one of those variables is overridden below.

# Run the whole suite against a ClickHouse started and torn down here.
test-live:
    #!/usr/bin/env bash
    set -euo pipefail
    dir=$(mktemp -d /tmp/ocaml-chc-test.XXXXXX)
    mkdir -p "$dir"/{data,logs,tmp,user_files,access}
    cat > "$dir/config.xml" <<EOF
    <clickhouse>
        <logger><level>warning</level><log>$dir/logs/server.log</log><errorlog>$dir/logs/error.log</errorlog><console>0</console></logger>
        <tcp_port>{{ port }}</tcp_port>
        <listen_host>127.0.0.1</listen_host>
        <path>$dir/data/</path>
        <tmp_path>$dir/tmp/</tmp_path>
        <user_files_path>$dir/user_files/</user_files_path>
        <access_control_path>$dir/access/</access_control_path>
        <mark_cache_size>536870912</mark_cache_size>
        <users><default><password></password><networks><ip>::/0</ip></networks><profile>default</profile><quota>default</quota></default></users>
        <profiles><default></default></profiles>
        <quotas><default></default></quotas>
    </clickhouse>
    EOF
    clickhouse server --config-file="$dir/config.xml" > "$dir/stdout.log" 2>&1 &
    pid=$!
    trap 'kill $pid 2>/dev/null || true; wait $pid 2>/dev/null || true; rm -rf "$dir"' EXIT
    for _ in $(seq 1 30); do
        if clickhouse client --port {{ port }} --query "SELECT 1" > /dev/null 2>&1; then break; fi
        sleep 1
    done
    if ! clickhouse client --port {{ port }} --query "SELECT 1" > /dev/null 2>&1; then
        echo "server did not come up; logs follow" >&2
        cat "$dir/logs/error.log" "$dir/stdout.log" >&2 2>/dev/null || true
        exit 1
    fi
    echo "clickhouse on 127.0.0.1:{{ port }} ($dir)"
    CHC_TEST_HOST=127.0.0.1 \
    CHC_TEST_PORT={{ port }} \
    CHC_TEST_USER=default \
    CHC_TEST_PASSWORD= \
    CHC_TEST_DATABASE=default \
        dune test --force
