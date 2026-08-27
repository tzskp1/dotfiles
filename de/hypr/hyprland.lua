-- See https://wiki.hypr.land/Configuring/Start/
-- Input (kb_layout etc.) and the nvidia env vars are generated from Nix,
-- see de/default.nix.

------------------
---- MONITORS ----
------------------

-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
hl.monitor({
    output   = "",
    mode     = "5120x2880@60.00",
    position = "auto",
    scale    = 2,
})


---------------------
---- MY PROGRAMS ----
---------------------

local terminal = "alacritty"
local menu     = "bemenu-run --hb '#467b96' --hf '#dfdfdf' --tb '#467b96' --tf '#dfdfdf' --fn 'Hack 22' -p 'Run:'"


-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------

hl.env("XCURSOR_THEME", "Nordzy-cursors")
hl.env("XCURSOR_SIZE", "24")
hl.env("QT_QPA_PLATFORMTHEME", "qt6ct")
hl.env("MOZ_ENABLE_WAYLAND", "1")
hl.env("GDK_BACKEND", "wayland")
hl.env("QT_QPA_PLATFORM", "wayland")
hl.env("QT_WAYLAND_DISABLE_WINDOWDECORATION", "1")
hl.env("GTK_THEME", "Adwaita:dark")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("XDG_SESSION_DESKTOP", "Hyprland")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("_JAVA_AWT_WM_NONREPARENTING", "1")
hl.env("HYPRCURSOR_THEME", "Nordzy-cursors")
hl.env("HYPRCURSOR_SIZE", "24")


-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function()
    hl.exec_cmd("LC_ALL=C waybar")
    hl.exec_cmd([[dconf write /org/gnome/desktop/interface/cursor-theme "'Nordzy-cursors'"]])
    hl.exec_cmd("hyprctl setcursor Nordzy-cursors-hyprcursor 32")
    hl.exec_cmd("dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=hyprland XDG_SESSION_TYPE=wayland")
    hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP")
    hl.exec_cmd("hypridle")
end)


-----------------------
---- LOOK AND FEEL ----
-----------------------

-- See https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
    general = {
        gaps_in     = 2,
        gaps_out    = 3,
        border_size = 2,

        col = {
            active_border   = { colors = { "rgba(33ccffee)", "rgba(00ff99ee)" }, angle = 45 },
            inactive_border = "rgba(595959aa)",
        },

        layout = "master",

        -- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Tearing/
        allow_tearing = false,
    },

    decoration = {
        rounding = 3,

        blur = {
            enabled  = true,
            size     = 3,
            passes   = 1,
            vibrancy = 0.1696,
        },

        shadow = {
            enabled      = true,
            range        = 4,
            render_power = 3,
            color        = "rgba(1a1a1aee)",
        },
    },

    animations = {
        enabled = true,
    },

    -- See https://wiki.hypr.land/Configuring/Layouts/Dwindle-Layout/
    dwindle = {
        preserve_split = true,
    },

    -- See https://wiki.hypr.land/Configuring/Layouts/Master-Layout/
    master = {
        new_on_top = true,
    },

    misc = {
        force_default_wallpaper    = -1, -- Set to 0 or 1 to disable the anime mascot wallpapers
        allow_session_lock_restore = true,
        focus_on_activate          = true,
        -- Let the compositor itself wake the display from DPMS off, instead of
        -- depending solely on hypridle's on-resume hook.
        mouse_move_enables_dpms    = true,
        key_press_enables_dpms     = true,
    },
})

-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Animations/
hl.curve("myBezier", { type = "bezier", points = { { 0.05, 0.9 }, { 0.1, 1.05 } } })

hl.animation({ leaf = "windows",     enabled = true, speed = 7,  bezier = "myBezier" })
hl.animation({ leaf = "windowsOut",  enabled = true, speed = 7,  bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border",      enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "borderangle", enabled = true, speed = 8,  bezier = "default" })
hl.animation({ leaf = "fade",        enabled = true, speed = 7,  bezier = "default" })
hl.animation({ leaf = "workspaces",  enabled = true, speed = 6,  bezier = "default" })


---------------
---- INPUT ----
---------------

-- Per-device config, see https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/
hl.device({
    name        = "epic-mouse-v1",
    sensitivity = -0.5,
})


--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

-- See https://wiki.hypr.land/Configuring/Basics/Window-Rules/
hl.window_rule({ name = "alacritty-to-ws1", match = { class = "^(Alacritty)" }, workspace = "1" })
hl.window_rule({ name = "firefox-to-ws2",   match = { class = "^(firefox)" },   workspace = "2" })
hl.window_rule({ name = "emacs-to-ws3",     match = { class = "^(emacs)" },     workspace = "3" })


---------------------
---- KEYBINDINGS ----
---------------------

local mainMod = "SUPER"

-- See https://wiki.hypr.land/Configuring/Basics/Binds/
hl.bind(mainMod .. " + SHIFT + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + SHIFT + C", hl.dsp.window.close())
hl.bind(mainMod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + P", hl.dsp.exec_cmd(menu))

hl.bind(mainMod .. " + d", hl.dsp.window.resize({ x = -30, y = 0, relative = true }))
hl.bind(mainMod .. " + n", hl.dsp.window.resize({ x = 30, y = 0, relative = true }))
hl.bind(mainMod .. " + t", hl.dsp.window.cycle_next({ next = false }))
hl.bind(mainMod .. " + h", hl.dsp.window.cycle_next())

hl.bind(mainMod .. " + SHIFT + t", hl.dsp.window.swap({ prev = true }))
hl.bind(mainMod .. " + SHIFT + h", hl.dsp.window.swap({ next = true }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
    local key = i % 10 -- 10 maps to key 0
    hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

hl.bind(mainMod .. " + SPACE", hl.dsp.window.fullscreen())
