{ pkgs, compiler ? "ghc883" }:

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  haskellLib = pkgs.haskell.lib;

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = se: su: {

      gi-gtk-declarative = haskellLib.markUnbroken (su.gi-gtk-declarative.overrideAttrs (oldAttrs: {
        doCheck = false;
      }));

      gi-gtk-declarative-app-simple = haskellLib.markUnbroken su.gi-gtk-declarative-app-simple;

      "nixos-manager" = haskellLib.overrideCabal
        (se.callCabal2nix "nixos-manager" (gitignore ./.) {})
        (drv: {
          buildTools = drv.buildTools or [] ++ [ pkgs.makeWrapper ];
          postFixup = ''
            wrapProgram $out/bin/nixos-manager \
            --prefix PATH : "${pkgs.lib.makeBinPath [pkgs.gksu]}"
          '';
        });
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
      cabal-install
      hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };
}
