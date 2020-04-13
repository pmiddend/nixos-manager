{ config, pkgs, ... }: {
  environment.systemPackages = [ pkgs.cheese ];
  fonts.fonts = [ pkgs.source-code-pro ];
  programs.light.enable = true;
}
