.PHONY: build test fmt fmt-check clean

build:
	dune build @all

test:
	dune test

# dune fmt covers .ml/.mli and dune files; clang-format covers the stubs.
# vendor/ carries a DisableFormat .clang-format, so it is left untouched.
fmt:
	dune fmt || true
	clang-format -i lib/chc/*.c

fmt-check:
	dune build @fmt
	clang-format --dry-run --Werror lib/chc/*.c

clean:
	dune clean
