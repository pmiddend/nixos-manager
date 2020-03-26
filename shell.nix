{ nixpkgs ? import ./nix/nixos-unstable.nix }:
let
  overlay = self: super: {
    myHaskellPackages =
      super.haskell.packages.ghc883.override (old: {
        overrides = self.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
            ghcWithPackages = hself.ghc.withPackages;
            gi-gtk-declarative = hsuper.gi-gtk-declarative.overrideAttrs (oldAttrs: {
              doCheck = false;
              # why doesn't this work?
              broken = false;
            });
          });
      });
  };

  pkgs = import nixpkgs {
    overlays = [overlay];
  };

  drv = pkgs.myHaskellPackages.callCabal2nix "nix-manager" ./nix-manager.cabal {};

  drvWithTools = drv.env.overrideAttrs (
    old: with pkgs.myHaskellPackages; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        brittany cabal-install hlint pkgs.gksu
      ];
    }
  );
in
  drvWithTools
