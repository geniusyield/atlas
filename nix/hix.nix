{pkgs, config, ...}: {
  # name = "project-name";
  compiler-nix-name = "ghc928"; # Version of GHC to use

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
      cabal = "3.10.1.0";
      hlint = "3.6.1";
      haskell-language-server = { version = "2.0.0.0"; index-state = "2023-06-05T06:39:32Z"; };
    };

    buildInputs = with pkgs; [
      jq
    ];
    shellHook = ''
      export CABAL_DIR=$(pwd)/.cabal
    '';
  };
}
