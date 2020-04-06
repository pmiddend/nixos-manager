let pkgs = import <nixpkgs> {};
in (import ./build.nix { inherit pkgs; })
