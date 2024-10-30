# How to Install
```bash
    cd /path/to/dotfiles
    nix run
    # for hyprlock
    echo 'auth include login' | sudo tee /etc/pam.d/hyprlock
    # for screen share
    sudo pacman -S xdg-desktop-portal-gtk
    # change login shell
    sudo -S chsh -s $(which zsh)
```

# Update
```bash
nix flake update --commit-lock-file
```
