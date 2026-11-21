{ inputs, vars, ... }:

{
  imports = [
    inputs.dms.homeModules.dank-material-shell
    inputs.dms-plugin-registry.homeModules.default
    inputs.danksearch.homeModules.dsearch
  ];

  home.sessionVariables = {
    DMS_POPOUT_LAYER = "overlay";
    DMS_DANKBAR_LAYER = "top";
  };

  programs = {
    # Dux Modular Shell (DMS)
    dank-material-shell = {
      enable = true;
      systemd.enable = true;
      enableSystemMonitoring = true;
      enableVPN = true;
      enableDynamicTheming = true;
      enableAudioWavelength = true;
      enableCalendarEvents = true;
      managePluginSettings = true;
      settings = {
        runUserMatugenTemplates = true;
        maxWorkspaceIcons = 1;
        showWorkspaceApps = false;
        currentThemeName = "dynamic";
        currentThemeCategory = "dynamic";
        cornerRadius = 11;
        fontFamily = vars.font;
        fontScale = 1.0;
        iconTheme = "Flat-Remix-Black-Dark";
        barConfigs = [
          {
            id = "default";
            name = "Main Bar";
            enabled = true;
            position = 0;
            screenPreferences = [ "all" ];
            showOnLastDisplay = true;
            leftWidgets = [ "launcherButton" "workspaceSwitcher" "focusedWindow" ];
            centerWidgets = [ "music" "clock" "weather" "intervalCommand:Org Clock" ];
            rightWidgets = [ "systemTray" "clipboard" "cpuUsage" "memUsage" "notificationButton" "battery" "controlCenterButton" ];
            spacing = 0;
            innerPadding = 6;
            bottomGap = 0;
            transparency = 1;
            widgetTransparency = 1;
            squareCorners = true;
            noBackground = false;
            gothCornersEnabled = true;
            gothCornerRadiusOverride = false;
            gothCornerRadiusValue = 15;
            borderEnabled = false;
            borderColor = "surfaceText";
            borderOpacity = 1;
            borderThickness = 1;
            fontScale = 1.0;
            autoHide = false;
            autoHideDelay = 250;
            openOnOverview = false;
            visible = true;
            popupGapsAuto = true;
            popupGapsManual = 4;
            widgetOutlineEnabled = false;
            widgetPadding = 9;
          }
        ];
      };
      plugins = {
        intervalCommand = {
          enable = true;
          settings = {
            variants = [
              {
                id = "Org Clock";
                name = "Org Clock";
                command = "${vars.scripts}/org-clock.nu";
                clickCommand = "${vars.scripts}/busybar.py toggle";
                icon = "clock";
                refreshInterval = 30;
              }
            ];
          };
        };
      };
    };

    # Dux Search (dsearch)
    dsearch = {
      enable = true;
      config = {
        max_file_bytes = 5242880; # 5MB
        worker_count = 8;

        index_paths = [
          {
            path = "~/Dokumentoj";
            max_depth = 0; # No limit
            exclude_hidden = false;
            exclude_dirs = [ ".git" ];
          }
          {
            path = "~/Programaroj";
            max_depth = 3;
            exclude_hidden = true;
            exclude_dirs = [ "node_modules" "venv" "target" ".git" "dist" "build" ];
          }
        ];
      };
    };
  };
}
