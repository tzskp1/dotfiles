{ username, useNvidia, kb_layout, kb_variant }: { config, pkgs, ... }:
let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  nixglhypr = import ./nixglhypr useNvidia pkgs;
  wrapGL = package: prog:
    pkgs.symlinkJoin
      {
        name = package.name;
        version = package.version;
        paths = [
          package
          nixglhypr
        ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          mv $out/bin/${prog} $out/bin/${prog}-orig
          chmod +x $out/bin/nixglhypr
          makeWrapper $out/bin/nixglhypr $out/bin/${prog} \
            --add-flags $out/bin/${prog}-orig \
            --inherit-argv0
        '';
      };
in
rec {
  home =
    let
      homeDirPrefix = if isDarwin then "/Users" else "/home";
      hypr = with pkgs;
        if isDarwin then [ ] else [
          bemenu
          waybar
          swayidle
          swaylock
          (wrapGL hyprland "Hyprland")
        ];
      sshrc = import ./sshrc pkgs;
    in
    {
      inherit username;
      packages = with pkgs; [
        tree-sitter # for emacs
        source-han-code-jp
        hack-font
        silver-searcher
        peco
        sshrc
      ] ++ hypr;
      homeDirectory = "/${homeDirPrefix}/${username}";
      stateVersion = "23.11";
    };

  programs.home-manager.enable = true;

  programs.less = {
    enable = true;
    keys = ''
      m               repeat-search
      \em             repeat-search-all
      M               reverse-search
      \eM             reverse-search-all
      h               forw-line
      t               back-line
      H               forw-line-force
      T               back-line-force
    '';
  };
  xdg.configFile."peco/config.json".text = ''
    {
        "keymap": {
            "C-h": "peco.SelectDown",
            "C-t": "peco.SelectUp",
            "C-d": "peco.ScrollPageDown",
            "C-n": "peco.ScrollPageUp",
            "C-b": "peco.DeleteBackwardChar"
        }
    }
  '';
  programs.neovim = {
    enable = true;
    vimAlias = true;
    withPython3 = true;
    extraConfig = ''
      noremap d h
      noremap h gj
      noremap t gk
      noremap n l
      inoremap <silent> hh <ESC>

      inoremap <C-c> <ESC>
      noremap! <C-g> <C-c>

      noremap k d
      noremap K D
      noremap M N
      noremap m n
      noremap! <C-b> <BS>

      noremap! <C-h> <Down>
      noremap! <C-t> <Up>
      noremap! <C-d> <Left>
      noremap! <C-n> <Right>

      let mapleader = "\<Space>"
      nnoremap <Leader>w :w<CR>
      nnoremap <Leader>q :q<CR>
      nnoremap <Leader>b :Denite buffer<CR>
      nnoremap <Leader>f :DeniteBufferDir file_rec<CR>

      augroup netrw_dvorak_fix
          autocmd!
          autocmd filetype netrw call Fix_netrw_maps_for_dvorak()
      augroup END
      function! Fix_netrw_maps_for_dvorak()
          noremap <buffer> d h
          noremap <buffer> h gj
          noremap <buffer> t gk
          noremap <buffer> n l
          noremap <buffer> e t
          noremap <buffer> l n
          " and any others...
      endfunction

      set clipboard=unnamedplus

      set hidden

      set number
      set relativenumber

      set smartcase
      set expandtab
      set tabstop=4
      set shiftwidth=4
      set autoindent
      set mouse=a
    '';
    plugins = with pkgs.vimPlugins; [
      molokai
    ];
  };

  xdg.configFile."waybar" = { source = ./de/waybar; recursive = true; };
  xdg.configFile."hypr/hyprland.conf".text =
    let
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
    inputSettings + (builtins.readFile ./de/hypr/hyprland.conf) + nvidiaSettings;
  xdg.configFile."hypr/scripts" = { source = ./de/hypr/scripts; recursive = true; };

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
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = if isDarwin then pkgs.emacs else pkgs.emacs-pgtk;
      config = ./emacs/init.el;
    };
    # TODO: Refactoring
    extraConfig = builtins.readFile ./emacs/init.el;
  };
  xdg.configFile."emacs/snippets" = { source = ./emacs/snippets; recursive = true; };

  programs.zsh = {
    enable = true;
    autocd = true;
    enableCompletion = true;
    autosuggestion.enable = false;
    syntaxHighlighting.enable = true;

    shellAliases = {
      ls = "ls --color=always";
      grep = "grep --color=auto";
      diff = "diff --color=auto";
    };
    history = {
      size = 100000;
      save = 100000;
      share = true;
      ignoreAllDups = true;
    };
    envExtra = ''
      export PAGER="less"
      export EDITOR="nvim"
      export LESS="-R"
      export LANG=en_US.UTF-8
    '';
    initExtraFirst = ''
      autoload -U edit-command-line
      zle -N edit-command-line
      bindkey "^X^E" edit-command-line
      bindkey -M viins "^B" vi-backward-delete-char
      bindkey -M viins "^T" up-line-or-history
      bindkey -M viins "^H" down-line-or-history
      bindkey -M viins "hh" vi-cmd-mode
      bindkey -M viins "^[OC" vi-forward-char
      bindkey -M viins "^[OD" vi-backward-char
      bindkey -M viins "^G" send-break
      bindkey -M vicmd "n" vi-forward-char
      bindkey -M vicmd "d" vi-backward-char
      bindkey -M vicmd "k" vi-delete
      zle -A .backward-kill-word vi-backward-kill-word
      zle -A .backward-delete-char vi-backward-delete-char
    '';
    initExtra = ''
      umask 002
      setopt no_beep
      unsetopt BEEP
      setopt correct
      setopt magic_equal_subst
      setopt auto_list
      setopt auto_menu
      setopt list_packed
      setopt list_types
      bindkey "^[[Z" reverse-menu-complete
      zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

      function peco-history-selection() {
          BUFFER=$(history -n 1 | tac | awk '!a[$0]++' | peco)
          CURSOR=$#BUFFER
          zle reset-prompt
      }
      zle -N peco-history-selection
      bindkey '^R' peco-history-selection

      function send_emacs(){
          emacsclient $1 > /dev/null &
      }
      alias e=send_emacs

      function extract() {
        case $1 in
          *.tar.gz|*.tgz) tar xzvf $1;;
          *.tar.xz) tar Jxvf $1;;
          *.zip) unzip $1;;
          *.lzh) lha e $1;;
          *.tar.bz2|*.tbz) tar xjvf $1;;
          *.tar.Z) tar zxvf $1;;
          *.gz) gzip -d $1;;
          *.bz2) bzip2 -dc $1;;
          *.Z) uncompress $1;;
          *.tar) tar xvf $1;;
          *.arj) unarj $1;;
          *.rar) unrar x $1;;
        esac
      }

      function cd() {
          builtin cd $@ && ls --color;
      }
    '';
    plugins = [
      {
        name = "pure";
        src = pkgs.fetchFromGitHub {
          owner = "sindresorhus";
          repo = "pure";
          rev = "v1.23.0";
          sha256 = "sha256-BmQO4xqd/3QnpLUitD2obVxL0UulpboT8jGNEh4ri8k=";
        };
      }
    ];
  };
  programs.dircolors = {
    enable = true;
    enableZshIntegration = true;
  };

  xdg.configFile.".sshrc" = { source = ./sshrc/.sshrc; };
  xdg.configFile.".sshrc.d" = { source = ./sshrc/.sshrc.d; recursive = true; };
}
