{
  description = "ocaml-chc — OCaml bindings to clickhouse-c (ClickHouse Native format)";

  inputs.nixpkgs.url = "https://flakehub.com/f/DeterminateSystems/nixpkgs-weekly/*.tar.gz";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];
      forAll = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
    in
    {
      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.ocaml
            pkgs.dune_3
            pkgs.ocamlformat
            pkgs.opam
            pkgs.pkg-config
            pkgs.gmp

            # clang-format for the C stubs; `make fmt` drives both formatters.
            pkgs.clang-tools

            # `clickhouse local --format Native` generates the wire bytes the
            # test suite decodes. Cached for aarch64-darwin — downloads, does
            # not build from source.
            pkgs.clickhouse
          ];
        };
      });
    };
}
