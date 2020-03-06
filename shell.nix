{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, brittany, bytestring
      , cabal-install, containers, directory, filepath, gi-gtk
      , haskell-gi-base, hlint, lens, lens-aeson, process, stdenv, text
      }:
      mkDerivation {
        pname = "nix-manager";
        version = "1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring containers directory filepath gi-gtk
          haskell-gi-base lens lens-aeson process text
        ];
        executableToolDepends = [ brittany cabal-install hlint ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
