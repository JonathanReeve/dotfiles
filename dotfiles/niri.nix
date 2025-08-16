{ config, pkgs, inputs, lib, ... }: {

  imports = [ inputs.niri.homeModules.niri ];

  options.app.niri.enable = lib.mkEnableOption "niri";

  config = lib.mkIf (config.app.niri.enable) {
    programs.niri = {
      enable = true;
      settings = {
        binds = with config.lib.niri.actions; let
	        sh = spawn "sh" "-c";

	      in {
	        # First Key Row
	        "Super+Space".action = spawn "${pkgs.rofi-wayland}/bin/rofi" "-show" "drun";
	        "Super+W".action = spawn "${pkgs.kitty}/bin/kitty";
	        "Super+E".action = spawn "${pkgs.chromium}/bin/chromium";
	        "Super+R".action = close-window;
	        "Super+H".action = spark "${pkgs.alacritty}/bin/alacritty";

          # Quit Niri
	        "Super+Shift+Q".action = quit;

	        # # Lock Session
	        # "Super+L".action = spawn "${pkgs.systemd}/bin/loginctl" "lock-session";

          # Screenshotting
	        "Print".action = screenshot;

          # Workspackes
	        "Super+0".action = focus-workspace 0;
	        "Super+1".action = focus-workspace 1;
	        "Super+2".action = focus-workspace 2;
	        "Super+3".action = focus-workspace 3;
	        "Super+4".action = focus-workspace 4;
	        "Super+5".action = focus-workspace 5;
	        "Super+6".action = focus-workspace 6;
	        "Super+7".action = focus-workspace 7;
	        "Super+8".action = focus-workspace 8;
	        "Super+9".action = focus-workspace 9;


	        # Special Keys
	        "XF86AudioRaiseVolume".action = sh "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+";
	        "XF86AudioLowerVolume".action = sh "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-";
	        "XF86AudioMute".action = sh "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle";
	      };
      };
    };
  };
}
