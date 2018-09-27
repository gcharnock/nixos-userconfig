{ pkgs, ... }:
{
  manual.html.enable = true;

  programs.termite = {
    enable = true;
    allowBold = true;
    backgroundColor = "rgba(0, 0, 0, 0.5)";
    font = "Monospace 9";
  };

  services.polybar = {
    enable = true;
    script = "polybar bar &";
    config = ./polybar.ini;
  };
}
