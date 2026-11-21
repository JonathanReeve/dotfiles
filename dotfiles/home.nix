{ pkgs, ... }:

let
  vars = {
    # Personal Info
    name = "Jonathan Reeve";
    email = "jonathan@jonreeve.com";
    githubUsername = "JonathanReeve";
    # Paths
    dokumentoj = "/home/jon/Dokumentoj";
    scripts = "/home/jon/Agordoj/scripts";
    maildir = "/home/jon/Retpoŝto";
    # Preferences
    font = "Victor Mono";
    # Binary paths
    emacsclient = "/etc/profiles/per-user/jon/bin/emacsclient";
  };

  mcpConfig = builtins.toJSON {
    mcpServers = {
      playwright = {
        command = "playwright-mcp";
        args = [ ];
      };
    };
  };
in
{
  _module.args.vars = vars;

  imports = [
    ./home/dms.nix
    ./home/doom-emacs.nix
    ./home/email.nix
    ./home/hyprland.nix
    ./home/qutebrowser.nix
    ./home/shell.nix
    ./home/theming.nix
  ];

  programs.home-manager.enable = true;

  # Dotfiles for the home root, ~/
  home = {
    keyboard = {
      options = [ "caps:escape" "esperanto:colemak" ];
      variant = "colemak";
    };
    # pointerCursor = {
    #   enable = true;
    #   package = pkgs.adwaita-icon-theme;
    #   name = "Adwaita";
    #   size = 24;
    #   gtk.enable = true;
    #   x11.enable = true;
    # };

    packages = with pkgs; [
      playwright-driver
      (writeShellScriptBin "playwright-mcp" ''
        export PLAYWRIGHT_BROWSERS_PATH="${pkgs.playwright-driver.browsers}"
        export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
        exec ${pkgs.nodejs}/bin/npx -y @playwright/mcp "$@"
      '')
    ];

    file = {
      ".stack/config.yaml".text = ''
        templates:
          params:
            author-name: ${vars.name}
            author-email: ${vars.email}
            github-username: ${vars.githubUsername}
      '';
      ".gemini/skills/" = {
        source = ../skills;
        recursive = true;
      };
      ".gemini/config/mcp_config.json".text = mcpConfig;
      ".gemini/antigravity-cli/mcp_config.json".text = mcpConfig;
    };
    stateVersion = "26.05";
  };

  systemd.user = { };

  xdg = {
    enable = true;
    desktopEntries = {
      org-protocol = {
        name = "Org Protocol Handler";
        exec = "emacsclient %u";
        icon = "emacs";
        type = "Application";
        mimeType = [ "x-scheme-handler/org-protocol" ];
      };
    };
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = "org.gnome.Evince.desktop";
        "text/html" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/org-protocol" = "org-protocol.desktop";
        "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
      };
    };
  };
}
