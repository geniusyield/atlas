{
  description = "atlas";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix?ref=2b68ab788d9c3536c696a1f3a5e5cd0cc25b4b7d";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.plutus.url = "github:input-output-hk/plutus?ref=c8d4364d0e639fef4d5b93f7d6c0912d992b54f9"; # used for libsodium-vrf

  outputs = { self, nixpkgs, haskell-nix, plutus }:
    let
      supportedSystems = [ "x86_64-linux" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          project = (nixpkgsFor system).haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc8107";
            projectFileName = "cabal.project";
            modules = [{
              packages = {
                plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                cardano-crypto-praos.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                cardano-crypto-class.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              };
            }];
            shell = {
              withHoogle = true;

              nativeBuildInputs = (with pkgs; [
                cabal-install
                hlint
                bashInteractive
                jq
                gnumake
                gnused
                nodePackages.npm
                nodePackages.webpack
                nodePackages.webpack-cli
              ]);

              tools = {
                haskell-language-server = { };
              };
            };
          };
        in project;
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake {});

      packages = perSystem (system: self.flake.${system}.packages);
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system:
        let pkgs = nixpkgsFor system;
            devShell = self.flake.${system}.devShell;
        in pkgs.lib.overrideDerivation (devShell) (oldAttrs: {
          buildInputs = pkgs.lib.lists.unique devShell.buildInputs;
        })
      );
    };
}
