{ useNvidia, kb_layout, kb_variant, ... }: { config, pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      dconf
      maestral
    ];
  };

  wayland.windowManager.hyprland = {
    enable = true;
    # set the flake package
    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
    settings = {
      cursor = {
        enable_hyprcursor = true;
        sync_gsettings_theme = true;
      };
    };
    extraConfig = let
      nvidiaSettings_ = ''
        env = LIBVA_DRIVER_NAME,nvidia
        env = XDG_SESSION_TYPE,wayland
        env = GBM_BACKEND,nvidia-drm
        env = __GLX_VENDOR_LIBRARY_NAME,nvidia
        env = WLR_NO_HARDWARE_CURSORS,1
      '';
      nvidiaSettings = if useNvidia then nvidiaSettings_ else "";
      inputSettings = ''
        # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
        input {
            kb_layout = ${kb_layout}
            kb_variant = ${kb_variant}
            kb_model =
            kb_options =
            kb_rules =

            follow_mouse = 1

            repeat_rate = 50
            repeat_delay = 300

            sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
        }
      '';
    in
      inputSettings + (builtins.readFile ./hypr/hyprland.conf) + nvidiaSettings;
  };

  programs.waybar.enable = true;
  xdg.configFile."waybar" = { source = ./waybar; recursive = true; };

  programs.bemenu.enable = true;

  programs.hyprlock = {
    enable = true;
    settings = {
      background = {
        # path = /home/me/someImage.png   # supports png, jpg, webp (no animations, though)
        color = "rgba(25, 20, 20, 1.0)";
        # all these options are taken from hyprland, see https://wiki.hyprland.org/Configuring/Variables/#blur for explanations
        # 0 disables blurring
        blur_passes = 0;
        blur_size = 7;
        noise = 0.0117;
        contrast = 0.8916;
        brightness = 0.8172;
        vibrancy = 0.1696;
        vibrancy_darkness = 0.0;
      };
      input-field = {
        size = "200, 50";
        outline_thickness = 3;
        # Scale of input-field height, 0.2 - 0.8
        dots_size = 0.33;
        # Scale of dots' absolute size, 0.0 - 1.0
        dots_spacing = 0.15;
        dots_center = false;
        # -1 default circle, -2 follow input-field rounding
        dots_rounding = -1;
        outer_color = "rgb(151515)";
        inner_color = "rgb(200, 200, 200)";
        font_color = "rgb(10, 10, 10)";
        fade_on_empty = true;
        # Milliseconds before fade_on_empty is triggered.
        fade_timeout = 1000;
        # Text rendered in the input box when it's empty.
        placeholder_text = "<i>Input Password...</i>";
        hide_input = false;
        # -1 means complete rounding (circle/oval)
        rounding = -1;
        check_color = "rgb(204, 136, 34)";
        # if authentication failed, changes outer_color and fail message color
        fail_color = "rgb(204, 34, 34)";
        # can be set to empty
        fail_text = "<i>$FAIL <b>($ATTEMPTS)</b></i>";
        # milliseconds before fail_text and fail_color disappears
        fail_timeout = 2000;
        # transition time in ms between normal outer_color and fail_color
        fail_transition = 300;
        capslock_color = -1;
        numlock_color = -1;
        # when both locks are active. -1 means don't change outer color (same for above)
        bothlock_color = -1;
        # change color if numlock is off
        invert_numlock = false;
        # see below
        swap_font_color = false;
        position = "0, -20";
        halign = "center";
        valign = "center";
      };
    };
  };

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        # avoid starting multiple hyprlock instances.
        lock_cmd = "pidof hyprlock || hyprlock";
      };
      listener = [
        {
          # 2.5min.
          timeout = 150;
          # screen off when timeout has passed
          on-timeout = "hyprctl dispatch dpms off";
          # screen on when activity is detected after timeout has fired.
          on-resume = "hyprctl dispatch dpms on";
        }
        {
          # 5min
          timeout = 300;
          # lock screen when timeout has passed
          on-timeout = "hyprlock";
        }
      ];
    };
  };

  # https://github.com/guillaumeboehm/Nordzy-cursors-hyprcursor
  xdg.dataFile."icons/Nordzy-cursors" = { source = ./Nordzy-cursors; recursive = true; };
  xdg.dataFile."icons/Nordzy-cursors-hyprcursor" = { source = ./Nordzy-cursors-hyprcursor; recursive = true; };
  xdg.configFile."gtk-4.0/settings.ini".text = ''
    [Settings]
    gtk-cursor-theme-name=Nordzy-cursors
    gtk-cursor-theme-size=24
  '';

  services.hyprpaper.enable = true;
  xdg.configFile."hypr/wallpaper.png".source = ./wallpaper.png;
  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = ${config.home.homeDirectory}/${config.xdg.configFile."hypr/wallpaper.png".target}
    wallpaper = , ${config.home.homeDirectory}/${config.xdg.configFile."hypr/wallpaper.png".target}
  '';

  programs.alacritty = {
    enable = true;
    settings =
      {
        font.size = 15;
        font.bold = {
          family = "Source Han Code JP N";
          style = "Bold";
        };
        font.normal = {
          family = "Source Han Code JP N";
          style = "Regular";
        };
      };
  };
}
