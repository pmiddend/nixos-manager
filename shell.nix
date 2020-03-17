{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, brittany, bytestring
      , cabal-install, containers, directory, filepath, gi-gtk
      , gi-gtk-declarative, gi-gtk-declarative-app-simple
      , haskell-gi-base, hlint, lens, lens-aeson, process, stdenv, text
      , megaparsec, data-fix, gi-gdk, pipes, regex-pcre, regex-compat
      }:
      mkDerivation {
        pname = "nix-manager";
        version = "1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring containers directory filepath gi-gtk
          gi-gtk-declarative gi-gtk-declarative-app-simple haskell-gi-base
          lens lens-aeson process text megaparsec data-fix gi-gdk pipes regex-pcre regex-compat
        ];
        executableToolDepends = [ brittany cabal-install hlint ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages_ = if compiler == "default"
                       then pkgs.haskellPackages
                     else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages_.override {
    overrides = self: super: {
      gi-gtk-declarative = super.gi-gtk-declarative.overrideAttrs (oldAttrs: {
        doCheck = false;
        # why doesn't this work?
        broken = false;
      });
      gi-gtk-declarative-app-simple = super.gi-gtk-declarative-app-simple.overrideAttrs (oldAttrs: {
        doCheck = false;
        # why doesn't this work?
        broken = false;
      });
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
