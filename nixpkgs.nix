let
  nixpkgs = import (builtins.fetchTarball
    {  # Descriptive name to make the store path easier to identify
      name = "nixos-unstable-2024-12-08";
      # git ls-remote https://github.com/nixos/nixpkgs <channel>
      url = "https://github.com/nixos/nixpkgs/archive/1544a19df5d06bbce4ee578e9ec55515454f6b7f.tar.gz";
      # nix-prefetch-url --unpack <url>
      sha256 = "sha256:062320xxpi3q70hl5mx8hzyzj1brvwgr34r7qxdai5wj9sk27r59";
    }) ;
  # local = import <nixpkgs>;


  # Config to inport external packages
  config.packageOverrides = pkgs: rec { haskellPackages =
    let mkDerivation = expr: pkgs.haskellPackages.mkDerivation (expr // {
          enableSeparateDocOutput = true;
          doHaddock = true;
          doCheck = true;
        });

    in
    pkgs.haskellPackages.override { overrides = hpkgs: opkgs:{
       orthori = import (builtins.fetchGit {
         url = "git@github.com:vcanadi/orthori";
         rev = "8fe9950218280536b6dd171f2a6a823232009119";
        }) { inherit pkgs hpkgs mkDerivation; };
      };
    };
  };
in nixpkgs { inherit config; }
