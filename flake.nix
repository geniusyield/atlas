{
  description = "atlas-cardano";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"	"x86_64-darwin"	"aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlay = final: prev: {
          haskell-nix = prev.haskell-nix // {
            extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
                # String pkgconfig-depends names are mapped to lists of Nixpkgs
                # package names
                "libblst" = [ "blst" ];
            };
          };
        };

        overlays = [ haskellNix.overlay
          (final: prev: {
            webkitgtk = final.webkitgtk_4_0;
            libsodium = with final; stdenv.mkDerivation rec {
              pname = "libsodium";

              src = fetchGit {
                url = "https://github.com/IntersectMBO/libsodium";
                rev = version;
              };
              version = "dbb48cce5429cb6585c9034f002568964f1ce567";

              nativeBuildInputs = [ autoreconfHook ];

              configureFlags = [ "--enable-static" ]
                # Fixes a compilation failure: "undefined reference to `__memcpy_chk'". Note
                # that the more natural approach of adding "stackprotector" to
                # `hardeningDisable` does not resolve the issue.
                ++ lib.optional stdenv.hostPlatform.isMinGW "CFLAGS=-fno-stack-protector";

              outputs = [ "out" "dev" ];
              separateDebugInfo = stdenv.isLinux && stdenv.hostPlatform.libc != "musl";

              enableParallelBuilding = true;

              doCheck = true;

              meta = with lib; {
                description = "A modern and easy-to-use crypto library - VRF fork";
                homepage = "http://doc.libsodium.org/";
                license = licenses.isc;
                maintainers = [ "tdammers" "nclarke" ];
                platforms = platforms.all;
              };
            };
          })
          (final: prev: {
            hixProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc966";
                # This is used by `nix develop .` to open a shell for use with
                # `cabal`, `hlint` and `haskell-language-server`
                shell.tools = {
                  cabal = {};
                  hlint = {};
                  haskell-language-server = {};
                  fourmolu = {};
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with final; [
                  nixpkgs-fmt
                ];
                # ???: Fix for `nix flake show --allow-import-from-derivation`
                evalSystem = "x86_64-linux";
                inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
              };
          })
          overlay
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
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
