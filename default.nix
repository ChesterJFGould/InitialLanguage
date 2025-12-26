let
  pkgs = import <nixpkgs>{};
in
  pkgs.haskell.packages.ghc912.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
  }
