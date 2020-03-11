{ config, pkgs, ... }:

{
  fonts.fonts = [ pkgs.source-code-pro ];
  programs.light.enable = true;

  environment.systemPackages = [
    pkgs.plantuml
    pkgs.usbutils
  ];
}
