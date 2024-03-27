{pkgs, config, ...}:
  let indexState = "2024-03-15T17:07:52Z";
  in {
  # name = "project-name";
  compiler-nix-name = "ghc964"; # Version of GHC to use

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell = {
    tools = {
      cabal = { version = "3.10.1.0"; index-state = indexState; };
      cabal-fmt = { index-state = indexState; };
      haskell-language-server = { index-state = indexState; };
    };

    buildInputs = with pkgs; [
      jq
    ];
  };
}
