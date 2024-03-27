{pkgs, config, ...}:
  {
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
      cabal = { };
      cabal-fmt = { };
      haskell-language-server = { };
      fourmolu = { };
      hlint = { };
    };

    buildInputs = with pkgs; [
      jq
      haskellPackages.pretty-simple
    ];

    # This force cabal to forget about the default folder.
    shellHook = ''
      export CABAL_DIR=$(pwd)/.cabal
    '';
  };
}
