{ pkgs, ... }:
{
  manual.html.enable = true;

  programs.termite = {
    enable = true;
    allowBold = true;
    backgroundColor = "rgba(0, 0, 0, 0.8)";
    font = "Monospace 9";
  };

  services.polybar = {
    enable = true;
    script = "polybar top &";
    config = ./polybar.ini;
    extraConfig = ''
      [module/workspaces-xmonad]
      type = custom/script
      exec = ${pkgs.coreutils}/bin/tail -F /tmp/.xmonad-workspace-log
      #exec-if = [ -p /tmp/.xmonad-workspace-log ]
      tail = true

      [module/title-xmonad]
      type = custom/script
      exec = ${pkgs.coreutils}/bin/tail -F /tmp/.xmonad-title-log
      # exec-if = [ -p /tmp/.xmonad-title-log ]
      tail = true
    '';
  };
}
