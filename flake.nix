{
  description = "atlas";
inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.plutus.url = "github:input-output-hk/plutus?ref=a56c96598b4b25c9e28215214d25189331087244"; # used for libsodium-vrf
  inputs.CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  outputs = { self, nixpkgs, flake-utils, haskellNix, plutus, CHaP }:
    let
      supportedSystems = [
        "x86_64-linux"	"x86_64-darwin"	"aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        plutusPkgs = (import plutus { inherit system; }).pkgs;
        overlays = [ haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = system;
                modules = [{
                  packages = {
                    cardano-crypto-praos.components.library.pkgconfig =
                      nixpkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];
                    cardano-crypto-class.components.library.pkgconfig =
                      nixpkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf plutusPkgs.secp256k1 ] ];
                  };
                }];
                inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;
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
