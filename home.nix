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
    config = {
      "bar/top" = {
        monitor = "\${env:MONITOR:VGA1}";
        width = "100%";
	height = "3%";
	radius = 0;
	modules-right = "date";
	modules-center = "xworkspaces";
      };
      "module/date" = {
        type = ''internal/date'';
        internal = 5;
        date = ''%d.%m.%y'';
        time = ''%H:%M'';
        label = ''%time%  %date%'';
      };
      "module/xworkspaces" = {
        type = "internal/xworkspaces";
      };
    };
  };
}
