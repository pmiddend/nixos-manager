{ pkgs, compiler ? "ghc883" }:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = se: su: {

      gi-gtk-declarative = pkgs.haskell.lib.markUnbroken (su.gi-gtk-declarative.overrideAttrs (oldAttrs: {
        doCheck = false;
      }));

      gi-gtk-declarative-app-simple = pkgs.haskell.lib.markUnbroken su.gi-gtk-declarative-app-simple;

      brotli = su.callPackage ./nix/brotli.nix { brotli-pkg = pkgs.brotli; };

      "nixos-manager" =
        se.callCabal2nix
          "nixos-manager"
          (gitignore ./.)
          {};
    };
  };

in
rec
{
  "nixos-manager" = myHaskellPackages."nixos-manager";
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."nixos-manager"
    ];
    buildInputs = with pkgs.haskellPackages; [
      brittany
      cabal-install
      hlint
      pkgs.gksu
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };
}
