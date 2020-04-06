let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in (import ./build.nix { inherit pkgs; }).shell
