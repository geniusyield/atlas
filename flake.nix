{
  description = "atlas-cardano";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.iohkNix = {
    url = "github:input-output-hk/iohk-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          iohkNix.overlays.crypto
          haskellNix.overlay
          iohkNix.overlays.haskell-nix-crypto
          iohkNix.overlays.haskell-nix-extra
          (final: prev: {
            lmdb-pkg-config = final.stdenvNoCC.mkDerivation {
              pname = "lmdb-pkg-config";
              version = final.lib.getVersion final.lmdb;
              dontUnpack = true;
              installPhase = ''
                mkdir -p "$out/lib/pkgconfig" "$out/nix-support"

                printf '%s\n' \
                  'prefix=${final.lmdb.out}' \
                  'includedir=${final.lmdb.dev}/include' \
                  'libdir=${final.lmdb.out}/lib' \
                  "" \
                  'Name: lmdb' \
                  'Description: Lightning Memory-Mapped Database' \
                  'Version: ${final.lib.getVersion final.lmdb}' \
                  'Cflags: -I${final.lmdb.dev}/include' \
                  'Libs: -L${final.lmdb.out}/lib -llmdb' \
                  > "$out/lib/pkgconfig/lmdb.pc"

                printf '%s\n' \
                  'export PKG_CONFIG_PATH='"$out"'/lib/pkgconfig''${PKG_CONFIG_PATH:+:}''${PKG_CONFIG_PATH-}' \
                  > "$out/nix-support/setup-hook"
              '';
            };
            hixProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc967";
                modules = [
                  {
                    packages."proto-lens-protobuf-types".components.library.build-tools = [ final.protobuf ];
                  }
                ];
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                  fourmolu = {};
                };
                shell.withHoogle = false;
                # Non-Haskell shell tools go here
                shell.buildInputs = with final; [
                  git
                  nixpkgs-fmt
                  lmdb
                  lmdb.dev
                  lmdb.out
                  lmdb-pkg-config
                  final.hixProject.hsPkgs.cardano-node.components.exes.cardano-node
                  final.hixProject.hsPkgs.cardano-cli.components.exes.cardano-cli
                ];
                shell.nativeBuildInputs = with final; [
                  pkg-config
                  lmdb-pkg-config
                  protobuf
                ];
                shell.shellHook = ''
                  export PKG_CONFIG_PATH=${final.lmdb-pkg-config}/lib/pkgconfig''${PKG_CONFIG_PATH:+:}''${PKG_CONFIG_PATH-}
                  export CARDANO_NODE="${final.hixProject.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node"
                  export CARDANO_CLI="${final.hixProject.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli"
                '';
                # ???: Fix for `nix flake show --allow-import-from-derivation`
                evalSystem = "x86_64-linux";
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
        # Built by `nix build .`
        packages.default = flake.packages."atlas-cardano:lib:atlas-cardano";
      });
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    allow-import-from-derivation = "true";
    bash-prompt-prefix = "[nix] ";
  };
}
