{ pkgs, compiler ? "ghc883" }:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = se: su: {
      gi-gtk-declarative = su.gi-gtk-declarative.overrideAttrs (oldAttrs: {
        doCheck = false;
        # why doesn't this work?
        broken = false;
      });

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
